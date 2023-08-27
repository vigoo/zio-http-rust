package zio.http.rust

import zio.{Chunk, IO, ZIO}
import zio.http.{Method, Status}
import zio.http.codec.{HttpCodec, HttpCodecType, PathCodec, SegmentCodec, SimpleCodec, TextCodec}
import zio.http.endpoint.*
import zio.http.rust.RustEndpoint.EndpointErrorCase
import zio.http.rust.printer.RustClient
import zio.prelude.*
import zio.prelude.fx.*
import zio.rust.codegen.ast.RustDef.ParameterModifier
import zio.rust.codegen.ast.{Crate, Name, RustDef, RustType}
import zio.rust.codegen.printer.Rust
import zio.schema.Schema
import zio.schema.rust.RustModel

final case class RustEndpoint(
    name: Name,
    method: String,
    pathSegments: Chunk[RustPathSegment],
    queryParameters: Chunk[RustParameter],
    headers: Chunk[RustParameter],
    bodies: Chunk[(Name, RustType)],
    requiredCrates: Set[Crate],
    outputs: Map[Status, RustType],
    errors: Map[Status, EndpointErrorCase],
    referredSchemas: Set[Schema[?]],
    knownErrorAdt: Option[Schema[?]]
):
  def ++(other: RustEndpoint): RustEndpoints =
    RustEndpoints(Name.fromString("Api"), Chunk(this, other))

  def allParameters: Chunk[(Name, RustType)] =
    val pathParams =
      pathSegments.collect:
        case RustPathSegment.Parameter(name, tpe) => name -> tpe
        case RustPathSegment.Trailing             => Name.fromString("path") -> RustType.ref(RustType.str)
    val queryParams =
      queryParameters.collect:
        case RustParameter.Parameter(name, tpe) => name -> tpe
    val headerParams =
      headers.collect:
        case RustParameter.Parameter(name, tpe) => name -> tpe

    pathParams ++ queryParams ++ bodies ++ headerParams

  def errorType: RustType = RustType.primitive(errorTypeName.asString)

  def errorTypeName: Name = name.toPascalCase + "Error"

  def resultType: RustType =
    RustType.result(
      outputs.head._2,
      errorType
    ) // TODO

  def successType: RustType = outputs.head._2

  def extraDefs: Chunk[RustDef] =
    val nonExternalErrors = errors.filterNot:
      case (_, EndpointErrorCase.ExternalType(_)) => true
      case _                                      => false
    Chunk(
      if outputs.size > 1 then
        Chunk(
          RustDef.`enum`(
            name.toPascalCase + "Success",
            outputs.map { case (status, tpe) =>
              RustDef.newtype(Name.fromString(s"Status${status.code}"), tpe)
            }.toSeq: _*
          )
        )
      else Chunk.empty,
      Chunk(
        RustDef.`enum`(
          errorTypeName,
          RustDef.newtype(Name.fromString("RequestFailure"), RustType.module("reqwest").primitive("Error")) +:
            RustDef.newtype(Name.fromString("InvalidHeaderValue"), RustType.module("reqwest", "header").primitive("InvalidHeaderValue")) +:
            RustDef.newtype(Name.fromString("UnexpectedStatus"), RustType.module("reqwest").primitive("StatusCode")) +:
            errors
              .map:
                case (status, EndpointErrorCase.ExternalType(tpe)) =>
                  RustDef.newtype(Name.fromString(s"Status${status.code}"), tpe)
                case (status, EndpointErrorCase.Inlined(fields, _, _, _, _)) =>
                  RustDef.pubStruct(Name.fromString(s"Status${status.code}"), fields: _*)
                case (status, EndpointErrorCase.Simple(_)) =>
                  RustDef.newtype(Name.fromString(s"Status${status.code}"), RustType.unit)
              .toSeq: _*
        ),
        RustDef.impl(
          RustType.parametric("From", RustType.module("reqwest").primitive("Error")),
          errorType,
          RustDef.fn(
            Name.fromString("from"),
            Chunk(
              RustDef.Parameter.Named(ParameterModifier.None, Name.fromString("error"), RustType.module("reqwest").primitive("Error"))
            ),
            errorType,
            errorTypeName.asString + "::RequestFailure(error)"
          )
        ),
        RustDef.impl(
          RustType.parametric("From", RustType.module("reqwest", "header").primitive("InvalidHeaderValue")),
          errorType,
          RustDef.fn(
            Name.fromString("from"),
            Chunk(
              RustDef.Parameter.Named(ParameterModifier.None, Name.fromString("error"), RustType.module("reqwest", "header").primitive("InvalidHeaderValue"))
            ),
            errorType,
            errorTypeName.asString + "::InvalidHeaderValue(error)"
          )
        )
      ),
      if nonExternalErrors.nonEmpty && knownErrorAdt.nonEmpty then
        val knownErrorAdtEnum = knownErrorAdt.get.asInstanceOf[Schema.Enum[?]]
        val knownErrorAdtName = Name.fromString(knownErrorAdtEnum.id.name) // TODO: no cast
        Chunk(
          RustDef.impl(
            errorType,
            RustDef.pubFn(
              (Name.fromString("to") ++ knownErrorAdtName).toSnakeCase,
              Chunk(RustDef.Parameter.Self(ParameterModifier.Ref)),
              RustType.optional(RustType.crate().module("model").primitive(knownErrorAdtName.asString)),
              toKnownErrorAdt(errorTypeName, errors, knownErrorAdtEnum)
            )
          )
        )
      else Chunk.empty,
      Chunk
        .fromIterable(errors)
        .collect:
          case (status, EndpointErrorCase.Inlined(fields, cname, errorAdt, errorAdtName, originalFields)) => (errorAdtName ++ cname + "Payload", originalFields)
        .map: (name, fields) =>
          RustDef
            .struct(name, fields: _*)
            .derive(RustType.debug)
            .derive(RustType.rustClone)
            .derive(RustType.serialize)
            .derive(RustType.deserialize)
    ).flatten

  def pathExpression: String =
    if pathSegments.forall(_.isLiteral) then
      """"""" +
        pathSegments
          .collect:
            case RustPathSegment.Literal(value) => value
          .mkString("/") + """""""
    else
      """&format!("""" +
        pathSegments
          .map:
            case RustPathSegment.Trailing           => "{path}"
            case RustPathSegment.Parameter(name, _) => "{" + name.toSnakeCase.asString + "}"
            case RustPathSegment.Literal(value)     => value
          .mkString("/") +
        """")"""

  def toEndpoints: RustEndpoints =
    RustEndpoints(Name.fromString("Api"), Chunk(this))

  private def toKnownErrorAdt(name: Name, errors: Map[Status, RustEndpoint.EndpointErrorCase], errorAdt: Schema.Enum[?]): String =
    RustClient.errorAdtConversion
      .printString(
        (
          name,
          errors.map((k, v) => (k.code, v)),
          RustType.crate().module("model").primitive(Name.fromString(errorAdt.id.name).asString)
        )
      )
      .toOption
      .get // TODO

object RustEndpoint:
  final case class PossibleOutput(tpe: RustType, status: Option[Status], isError: Boolean, schema: Schema[?])

  enum EndpointErrorCase:
    case Simple(constructorName: Name)
    case ExternalType(tpe: RustType)
    case Inlined(fields: Chunk[RustDef.Field], constructorName: Name, errorAdt: RustType, errorAdtName: Name, originalFields: Chunk[RustDef.Field])

  final case class State(
      method: String,
      pathSegments: Chunk[RustPathSegment],
      queryParameters: Chunk[RustParameter],
      headers: Chunk[RustParameter],
      bodies: Chunk[(Name, RustType)],
      referredSchemas: Set[Schema[?]],
      possibleOutputStack: List[PossibleOutput],
      outputs: Map[Status, RustType],
      errors: Map[Status, EndpointErrorCase],
      requiredCrates: Set[Crate],
      knownErrorAdt: Option[Schema[?]]
  ):
    def addBody(name: Name, tpe: RustType, schema: Schema[?]): State =
      copy(
        bodies = bodies :+ (name -> tpe),
        referredSchemas = referredSchemas + schema
      )

    def addHeader(name: Name, tpe: RustType): State =
      copy(headers = headers :+ RustParameter.Parameter(name, tpe))

    def addHeader(name: Name, value: String): State =
      copy(headers = headers :+ RustParameter.Static(name, value))

    def addPathSegment(segment: RustPathSegment): State =
      copy(pathSegments = pathSegments :+ segment)

    def addQueryParameter(name: Name, tpe: RustType): State =
      copy(queryParameters = queryParameters :+ RustParameter.Parameter(name, tpe))

    def addQueryParameter(name: Name, value: String): State =
      copy(queryParameters = queryParameters :+ RustParameter.Static(name, value))

    def addRequiredCrates(crates: Set[Crate]): State =
      copy(requiredCrates = requiredCrates ++ crates)

    def addResultType(tpe: RustType, schema: Schema[?]): State =
      copy(
        possibleOutputStack = possibleOutputStack.head.copy(tpe = tpe, schema = schema) :: possibleOutputStack.tail
      )

    def addStatus(status: Status): State =
      copy(
        possibleOutputStack = possibleOutputStack.head.copy(status = Some(status)) :: possibleOutputStack.tail
      )

    def startPossibleOutput(isError: Boolean): State =
      copy(possibleOutputStack = possibleOutputStack :+ PossibleOutput(RustType.unit, None, isError, Schema[Unit]))

    def finishPossibleOutput(): State =
      val PossibleOutput(tpe, status, isError, schema) = possibleOutputStack.head
      val newPossibleOutputStack = possibleOutputStack.tail
      status match
        case None => this
        case Some(status) =>
          val newOutputs = if !isError then outputs.updated(status, tpe) else outputs
          val isErrorCons = isError && isConstructorOfKnownErrorAdt(schema)
          val newErrors =
            if isError then
              if isErrorCons then
                schema match
                  case record: Schema.Record[?] =>
                    if record.fields.isEmpty then errors.updated(status, EndpointErrorCase.Simple(Name.fromString(record.id.name)))
                    else
                      val model = RustModel.fromSchema(record).toOption.get
                      val fields = model.definitions.collectFirst:
                        case RustDef.Struct(name, fields, _, _) if name == Name.fromString(record.id.name) =>
                          (fields.map(_.copy(attributes = Chunk.empty, isPublic = false)), fields)
                      errors.updated(
                        status,
                        EndpointErrorCase.Inlined(
                          fields.map(_._1).getOrElse(Chunk.empty),
                          Name.fromString(record.id.name),
                          knownErrorAdtType.get,
                          knownErrorAdtName.get,
                          fields.map(_._2).getOrElse(Chunk.empty)
                        )
                      )
                  case _ =>
                    errors.updated(status, EndpointErrorCase.Simple(Name.fromString("unknown")))
              else errors.updated(status, EndpointErrorCase.ExternalType(tpe))
            else errors
          val newReferredSchemas = if isErrorCons then referredSchemas else referredSchemas + schema
          copy(
            possibleOutputStack = newPossibleOutputStack,
            outputs = newOutputs,
            errors = newErrors,
            referredSchemas = newReferredSchemas
          )

    private def isConstructorOfKnownErrorAdt(schema: Schema[?]): Boolean =
      knownErrorAdt.exists:
        case sum: Schema.Enum[_] => sum.cases.exists(_.schema == schema)
        case _                   => false

    private def knownErrorAdtType: Option[RustType] =
      knownErrorAdtName.map: name =>
        RustType.crate().module("model").primitive(name.asString)

    private def knownErrorAdtName: Option[Name] =
      knownErrorAdt.flatMap:
        case sum: Schema.Enum[_] => Some(Name.fromString(sum.id.name))
        case _                   => None

  object State:
    val empty: State =
      State(
        method = "GET",
        pathSegments = Chunk.empty,
        queryParameters = Chunk.empty,
        headers = Chunk.empty,
        bodies = Chunk.empty,
        referredSchemas = Set.empty,
        possibleOutputStack = List.empty,
        outputs = Map.empty,
        errors = Map.empty,
        requiredCrates = Set.empty,
        knownErrorAdt = None
      )

  type Fx[+A] = ZPure[Nothing, State, State, Any, String, A]

  def fromEndpoint[PathInput, Input, Err, Output, Middleware <: EndpointMiddleware](
      name: String,
      endpoint: Endpoint[PathInput, Input, Err, Output, Middleware],
      knownErrorAdt: Option[Schema[?]] = None
  ): Either[String, RustEndpoint] =
    val builder =
      for {
        _ <- addInput(endpoint.input)
        _ <- updateState(_.startPossibleOutput(isError = false))
        _ <- addOutput(endpoint.output, isError = false)
        _ <- updateState(_.finishPossibleOutput())
        _ <- updateState(_.startPossibleOutput(isError = true))
        _ <- addOutput(endpoint.error, isError = true)
        _ <- updateState(_.finishPossibleOutput())
        state <- getState
      } yield state

    builder.provideState(State.empty.copy(knownErrorAdt = knownErrorAdt, referredSchemas = knownErrorAdt.toSet)).runEither.map(postProcess(name, _))

  private def postProcess(name: String, state: State): RustEndpoint =
    RustEndpoint(
      name = Name.fromString(name),
      method = state.method,
      pathSegments = state.pathSegments,
      queryParameters = state.queryParameters,
      headers = state.headers,
      bodies = state.bodies,
      referredSchemas = state.referredSchemas,
      outputs = state.outputs,
      errors = state.errors,
      requiredCrates = state.requiredCrates,
      state.knownErrorAdt
    )

  private def getState: Fx[State] = ZPure.get[State]

  private def updateState(f: State => State): Fx[Unit] =
    ZPure.update[State, State](f)

  private def tryUpdateState(f: State => Either[String, State]): Fx[Unit] =
    ZPure.get[State].flatMap(state => ZPure.fromEither(f(state))).flatMap(ZPure.set[State])

  private def addInput[Input](input: HttpCodec[HttpCodecType.RequestType, Input], optional: Boolean = false): Fx[Unit] =
    input match
      case HttpCodec.Combine(left, right, inputCombiner) =>
        addInput(left) *> addInput(right)
      case HttpCodec.Content(schema, mediaType, name, index) =>
        ZPure
          .fromEither(RustModel.fromSchema(schema))
          .flatMap: model =>
            val baseTypeRef = model.typeRefs(schema)
            val typeRef = if optional then RustType.optional(baseTypeRef) else baseTypeRef
            updateState(_.addBody(Name.fromString(name.getOrElse("body")), typeRef, schema))
      case HttpCodec.ContentStream(schema, mediaType, name, index) =>
        ZPure
          .fromEither(RustModel.fromSchema(schema))
          .flatMap: model =>
            val elemTypeRef = model.typeRefs(schema)
            val streamTypeRef = RustType.vec(elemTypeRef)
            val typeRef = if optional then RustType.optional(streamTypeRef) else streamTypeRef
            updateState(_.addBody(Name.fromString(name.getOrElse("body")), typeRef, schema))
      case HttpCodec.Empty =>
        ZPure.unit
      case HttpCodec.Fallback(left, HttpCodec.Empty) =>
        addInput(left, optional = true)
      case HttpCodec.Fallback(left, right) =>
        addInput(left) // NOTE: right not supported
      case HttpCodec.Halt =>
        ZPure.unit
      case HttpCodec.Header(name, codec, index) =>
        val opt = (rt: RustType) => if optional then RustType.optional(rt) else rt
        codec match
          case TextCodec.Constant(string) =>
            updateState(_.addHeader(Name.fromString(name), string))
          case TextCodec.StringCodec =>
            updateState(_.addHeader(Name.fromString(name), opt(RustType.ref(RustType.str))))
          case TextCodec.IntCodec =>
            updateState(_.addHeader(Name.fromString(name), opt(RustType.i32)))
          case TextCodec.LongCodec =>
            updateState(_.addHeader(Name.fromString(name), opt(RustType.i64)))
          case TextCodec.BooleanCodec =>
            updateState(_.addHeader(Name.fromString(name), opt(RustType.bool)))
          case TextCodec.UUIDCodec =>
            updateState(_.addHeader(Name.fromString(name), opt(RustType.uuid)).addRequiredCrates(Set(Crate.uuid)))
      case HttpCodec.Method(codec, index) =>
        codec match
          case SimpleCodec.Specified(value: Method) =>
            updateState(_.copy(method = value.name))
          case SimpleCodec.Unspecified() =>
            ZPure.fail("Unspecified method")
      case HttpCodec.Path(codec, index) =>
        addPath(codec)
      case HttpCodec.Query(name, codec, index) =>
        val opt = (rt: RustType) => if optional then RustType.optional(rt) else rt
        codec match
          case TextCodec.Constant(string) =>
            updateState(_.addQueryParameter(Name.fromString(name), string))
          case TextCodec.StringCodec =>
            updateState(_.addQueryParameter(Name.fromString(name), opt(RustType.ref(RustType.str))))
          case TextCodec.IntCodec =>
            updateState(_.addQueryParameter(Name.fromString(name), opt(RustType.i32)))
          case TextCodec.LongCodec =>
            updateState(_.addQueryParameter(Name.fromString(name), opt(RustType.i64)))
          case TextCodec.BooleanCodec =>
            updateState(_.addQueryParameter(Name.fromString(name), opt(RustType.bool)))
          case TextCodec.UUIDCodec =>
            updateState(_.addQueryParameter(Name.fromString(name), opt(RustType.uuid)).addRequiredCrates(Set(Crate.uuid)))
      case HttpCodec.Status(codec, index) =>
        ZPure.unit
      case HttpCodec.TransformOrFail(api, f, g) =>
        addInput(api)
      case HttpCodec.Annotated(codec, metadata) =>
        addInput(codec)

  private def addOutput[Output](output: HttpCodec[HttpCodecType.ResponseType, Output], isError: Boolean): Fx[Unit] =
    output match
      case HttpCodec.Combine(left, right, inputCombiner) =>
        for
          _ <- addOutput(left, isError)
          _ <- addOutput(right, isError)
        yield ()
      case HttpCodec.Content(schema, mediaType, name, index) =>
        ZPure
          .fromEither(RustModel.fromSchema(schema))
          .flatMap: model =>
            val typeRef = model.typeRefs(schema)
            // TODO: support multipart/form-data output
            updateState(
              _.addResultType(typeRef, schema)
            )
      case HttpCodec.ContentStream(schema, mediaType, name, index) =>
        ZPure
          .fromEither(RustModel.fromSchema(schema))
          .flatMap: model =>
            val typeRef = RustType.vec(model.typeRefs(schema))
            // TODO: support multipart/form-data output
            updateState(_.addResultType(typeRef, schema))
      case HttpCodec.Empty =>
        ZPure.unit
      case HttpCodec.Halt =>
        ZPure.unit
      case HttpCodec.Fallback(HttpCodec.Halt, right) =>
        addOutput(right, isError)
      case HttpCodec.Fallback(left, right) =>
        for
          _ <- updateState(_.startPossibleOutput(isError))
          _ <- addOutput(left, isError)
          _ <- addOutput(right, isError)
          _ <- updateState(_.finishPossibleOutput())
        yield ()
      case HttpCodec.Header(name, codec, index) =>
        ZPure.unit // TODO: support expected result headers
      case HttpCodec.Status(codec, index) =>
        codec match
          case SimpleCodec.Specified(value) =>
            updateState(_.addStatus(value))
          case SimpleCodec.Unspecified() =>
            ZPure.fail("Unspecified status")

      case HttpCodec.TransformOrFail(api, f, g) =>
        addOutput(api, isError)
      case HttpCodec.Path(codec, index) =>
        ZPure.fail("Unexpected codec in output position")
      case HttpCodec.Query(name, codec, index) =>
        ZPure.fail("Unexpected codec in output position")
      case HttpCodec.Method(codec, index) =>
        ZPure.fail("Unexpected codec in output position")
      case HttpCodec.Annotated(codec, metadata) =>
        addOutput(codec, isError)

  private def addPath[Input](codec: PathCodec[Input]): Fx[Unit] =
    codec match
      case PathCodec.Segment(segment, doc) =>
        segment match
          case SegmentCodec.Empty(doc) =>
            ZPure.unit
          case SegmentCodec.Literal(value, doc) =>
            updateState(_.addPathSegment(RustPathSegment.Literal(value)))
          case SegmentCodec.BoolSeg(name, doc) =>
            updateState(_.addPathSegment(RustPathSegment.Parameter(Name.fromString(name), RustType.bool)))
          case SegmentCodec.IntSeg(name, doc) =>
            updateState(_.addPathSegment(RustPathSegment.Parameter(Name.fromString(name), RustType.i32)))
          case SegmentCodec.LongSeg(name, doc) =>
            updateState(_.addPathSegment(RustPathSegment.Parameter(Name.fromString(name), RustType.i64)))
          case SegmentCodec.Text(name, doc) =>
            updateState(_.addPathSegment(RustPathSegment.Parameter(Name.fromString(name), RustType.ref(RustType.str))))
          case SegmentCodec.UUID(name, doc) =>
            updateState(_.addPathSegment(RustPathSegment.Parameter(Name.fromString(name), RustType.uuid)).addRequiredCrates(Set(Crate.uuid)))
          case SegmentCodec.Trailing(doc) =>
            updateState(_.addPathSegment(RustPathSegment.Trailing))
      case PathCodec.Concat(left, right, combiner, doc) =>
        addPath(left) *> addPath(right)
      case PathCodec.TransformOrFail(api, f, g) =>
        addPath(api)

  def withKnownErrorAdt[A](using Schema[A]): WithKnownErrorAdt =
    WithKnownErrorAdt(Schema[A])

  class WithKnownErrorAdt(knownErrorAdt: Schema[?]):
    def zio: [PathInput, Input, Err, Output, Middleware <: EndpointMiddleware] => (
        String,
        Endpoint[PathInput, Input, Err, Output, Middleware]
    ) => IO[String, RustEndpoint] =
      [PathInput, Input, Err, Output, Middleware <: EndpointMiddleware] =>
        (name: String, endpoint: Endpoint[PathInput, Input, Err, Output, Middleware]) =>
          ZIO.fromEither(RustEndpoint.fromEndpoint(name, endpoint, Some(knownErrorAdt)))
