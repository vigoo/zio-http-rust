package zio.http.rust

import zio.Chunk
import zio.http.Method
import zio.http.codec.{HttpCodec, HttpCodecType, PathCodec, SegmentCodec, SimpleCodec, TextCodec}
import zio.http.endpoint.*
import zio.prelude.*
import zio.prelude.fx.*
import zio.rust.codegen.ast.{Crate, Name, RustType}
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
    referredSchemas: Set[Schema[?]]
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

  def resultType: RustType =
    RustType.result(RustType.unit, RustType.module("reqwest").primitive("Error"))

  def toEndpoints: RustEndpoints =
    RustEndpoints(Name.fromString("Api"), Chunk(this))

object RustEndpoint:
  final case class State(
      method: String,
      pathSegments: Chunk[RustPathSegment],
      queryParameters: Chunk[RustParameter],
      headers: Chunk[RustParameter],
      bodies: Chunk[(Name, RustType)],
      referredSchemas: Set[Schema[?]],
      requiredCrates: Set[Crate]
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

  object State:
    val empty: State =
      State(
        method = "GET",
        pathSegments = Chunk.empty,
        queryParameters = Chunk.empty,
        headers = Chunk.empty,
        bodies = Chunk.empty,
        referredSchemas = Set.empty,
        requiredCrates = Set.empty
      )

  type Fx[+A] = ZPure[Nothing, State, State, Any, String, A]

  def fromEndpoint[PathInput, Input, Err, Output, Middleware <: EndpointMiddleware](
      name: String,
      endpoint: Endpoint[PathInput, Input, Err, Output, Middleware]
  ): Either[String, RustEndpoint] =
    val builder =
      for {
        _ <- addInput(endpoint.input)
        state <- getState
      } yield state

    builder.provideState(State.empty).runEither.map(postProcess(name, _))

  private def postProcess(name: String, state: State): RustEndpoint =
    RustEndpoint(
      name = Name.fromString(name),
      method = state.method,
      pathSegments = state.pathSegments,
      queryParameters = state.queryParameters,
      headers = state.headers,
      bodies = state.bodies,
      referredSchemas = state.referredSchemas,
      requiredCrates = state.requiredCrates
    )

  private def getState: Fx[State] = ZPure.get[State]
  private def updateState(f: State => State): Fx[Unit] =
    ZPure.update[State, State](f)

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
      case HttpCodec.WithDoc(in, doc) =>
        addInput(in)
      case HttpCodec.WithExamples(in, examples) =>
        addInput(in)

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
