package zio.schema.rust

import zio.Chunk
import zio.prelude.*
import zio.prelude.fx.*
import zio.rust.codegen.ast.{Crate, Name, RustAttribute, RustDef, RustType}
import zio.schema.{Schema, TypeId}

final case class RustModel(typeRefs: Map[Schema[?], RustType], definitions: Chunk[RustDef], requiredCrates: Set[Crate])

object RustModel:
  final case class Capabilities(ord: Boolean, hash: Boolean):
    def &&(other: Capabilities): Capabilities =
      Capabilities(ord = ord && other.ord, hash = hash && other.hash)

  final case class State(
      typeRefs: Map[Schema[?], RustType],
      definitions: Chunk[RustDef],
      requiredCrates: Set[Crate],
      processed: Set[Schema[?]],
      stack: Chunk[Schema[?]],
      nameTypeIdMap: Map[Name, Set[TypeId]],
      schemaCaps: Map[Schema[?], Capabilities]
  )

  type Fx[+A] = ZPure[Nothing, State, State, Any, String, A]

  val empty: State =
    State(
      typeRefs = Map.empty,
      definitions = Chunk.empty,
      requiredCrates = Set.empty,
      processed = Set.empty,
      stack = Chunk.empty,
      nameTypeIdMap = Map.empty,
      schemaCaps = Map.empty
    )

  def fromSchema[A](schema: Schema[A]): Either[String, RustModel] =
    val processSchema =
      for
        _ <- preprocess(schema)
        _ <- resetProcessed
        _ <- process(schema)
        state <- getState
      yield state
    processSchema.provideState(empty).runEither.map(postProcess)

  def fromSchemas(schemas: Seq[Schema[?]]): Either[String, RustModel] =
    val processSchemas =
      for
        _ <- schemas.forEach(preprocess)
        _ <- resetProcessed
        _ <- schemas.forEach(process)
        state <- getState
      yield state
    processSchemas.provideState(empty).runEither.map(postProcess)

  private def postProcess(model: State): RustModel =
    RustModel(
      typeRefs = model.typeRefs,
      definitions = model.definitions.distinct,
      requiredCrates = model.requiredCrates + Crate.serde
    )

  private def getState: Fx[State] = ZPure.get[State]
  private def updateState(f: State => State): Fx[Unit] = ZPure.update[State, State](f)

  private def resetProcessed: Fx[Unit] =
    updateState(s => s.copy(processed = Set.empty))

  private def stacked[A, R](schema: Schema[A])(f: => Fx[R]): Fx[R] =
    updateState(s => s.copy(stack = s.stack :+ schema))
      .zipRight(f)
      .zipLeft(updateState(s => s.copy(stack = s.stack.dropRight(1))))

  private def boxIfNeeded[A](schema: Schema[A]): Fx[RustType] =
    for
      state <- getState
      backRef = state.stack.contains(schema)
      rustType <- getRustType(schema)
    yield if backRef then RustType.box(rustType) else rustType

  private def recordTypeId(name: Name, typeId: TypeId): Fx[Unit] =
    updateState(s =>
      s.copy(
        nameTypeIdMap = s.nameTypeIdMap.updated(name, s.nameTypeIdMap.getOrElse(name, Set.empty) + typeId)
      )
    )

  private def getCaps(schema: Schema[?]): Fx[Option[Capabilities]] =
    getState.map: state =>
      state.schemaCaps.get(schema)

  private def getCapsOrDefault(schema: Schema[?]): Fx[Capabilities] =
    getCaps(schema).map(_.getOrElse(Capabilities(ord = false, hash = false)))

  private def storeCaps(schema: Schema[?], caps: Capabilities): Fx[Unit] =
    updateState: state =>
      state.copy(
        schemaCaps = state.schemaCaps.updated(
          schema,
          caps
        )
      )

  private def storeCapsBasedOn(schema: Schema[?], basedOn: Schema[?]): Fx[Unit] =
    updateState: state =>
      val baseCaps = state.schemaCaps.getOrElse(basedOn, Capabilities(ord = false, hash = false))
      state.copy(
        schemaCaps = state.schemaCaps.updated(
          schema,
          baseCaps
        )
      )

  private def preprocess[A](schema: Schema[A]): Fx[Unit] =
    ifNotProcessed(schema):
      schema match
        case sum: Schema.Enum[_] =>
          for
            _ <- recordTypeId(Name.fromString(sum.id.name), sum.id)
            _ <- sum.cases
              .forEach: constructor =>
                constructor.schema match
                  case record: Schema.Record[_] =>
                    record.fields.forEach(field => preprocess(field.schema))
                  case _ =>
                    preprocess(constructor.schema)
              .unit
            caseCaps <- sum.cases
              .forEach: constructor =>
                constructor.schema match
                  case record: Schema.Record[_] =>
                    record.fields
                      .forEach(field => getCapsOrDefault(field.schema))
                      .map: fieldCaps =>
                        fieldCaps.foldLeft(Capabilities(ord = true, hash = true))(_ && _)
                  case schema =>
                    getCapsOrDefault(schema)
            caps = caseCaps.foldLeft(Capabilities(ord = true, hash = true))(_ && _)
            _ <- storeCaps(sum, caps)
          yield ()

        case record: Schema.Record[_] =>
          for
            _ <- recordTypeId(Name.fromString(record.id.name), record.id)
            _ <- record.fields
              .forEach: field =>
                preprocess(field.schema)
              .unit
            fieldCaps <- record.fields.forEach(field => getCapsOrDefault(field.schema))
            caps = fieldCaps.foldLeft(Capabilities(ord = true, hash = true))(_ && _)
            _ <- storeCaps(record, caps)
          yield ()
        case map: Schema.Map[_, _] =>
          for
            _ <- preprocess(map.keySchema)
            _ <- preprocess(map.valueSchema)
            _ <- storeCaps(map, Capabilities(ord = false, hash = false))
          yield ()
        case seq: Schema.Sequence[_, _, _] =>
          for
            _ <- preprocess(seq.elementSchema)
            _ <- storeCapsBasedOn(seq, seq.elementSchema)
          yield ()
        case set: Schema.Set[_] =>
          for
            _ <- preprocess(set.elementSchema)
            _ <- storeCaps(set, Capabilities(ord = false, hash = false))
          yield ()
        case transform: Schema.Transform[_, _, _] =>
          for
            _ <- preprocess(transform.schema)
            _ <- storeCapsBasedOn(transform, transform.schema)
          yield ()
        case primitive: Schema.Primitive[_] =>
          storeCaps(primitive, Capabilities(ord = true, hash = true))
        case opt: Schema.Optional[_] =>
          for
            _ <- preprocess(opt.schema)
            _ <- storeCapsBasedOn(opt, opt.schema)
          yield ()
        case fail: Schema.Fail[_] =>
          ZPure.fail(fail.message)
        case tuple: Schema.Tuple2[_, _] =>
          for
            _ <- preprocess(tuple.left)
            _ <- preprocess(tuple.right)
            leftCaps <- getCapsOrDefault(tuple.left)
            rightCaps <- getCapsOrDefault(tuple.right)
            caps = leftCaps && rightCaps
            _ <- storeCaps(tuple, caps)
          yield ()
        case either: Schema.Either[_, _] =>
          for
            _ <- preprocess(either.left)
            _ <- preprocess(either.right)
            leftCaps <- getCapsOrDefault(either.left)
            rightCaps <- getCapsOrDefault(either.right)
            caps = leftCaps && rightCaps
            _ <- storeCaps(either, caps)
          yield ()
        case lzy: Schema.Lazy[_] =>
          for
            _ <- preprocess(lzy.schema)
            _ <- storeCapsBasedOn(lzy, lzy.schema)
          yield ()
        case dyn: Schema.Dynamic =>
          storeCaps(dyn, Capabilities(ord = false, hash = false))

  private def getRustType[A](schema: Schema[A]): Fx[RustType] =
    getState.flatMap: state =>
      val typeRef = state.typeRefs.get(schema) match
        case Some(rustType) =>
          ZPure.succeed(rustType)
        case None =>
          stacked(schema):
            schema match
              case sum: Schema.Enum[_] =>
                ZPure.succeed(RustType.crate().module("model").primitive(sum.id.name))
              case record: Schema.Record[_] =>
                ZPure.succeed(RustType.crate().module("model").primitive(record.id.name))
              case map: Schema.Map[_, _] =>
                for
                  key <- boxIfNeeded(map.keySchema)
                  value <- boxIfNeeded(map.valueSchema)
                yield RustType.hashMap(key, value)
              case seq: Schema.Sequence[_, _, _] =>
                boxIfNeeded(seq.elementSchema).map: elementType =>
                  RustType.vec(elementType)
              case set: Schema.Set[_] =>
                boxIfNeeded(set.elementSchema).map: elementType =>
                  RustType.hashSet(elementType)
              case transform: Schema.Transform[_, _, _] =>
                boxIfNeeded(transform.schema)
              case primitive: Schema.Primitive[_] =>
                ZPure.succeed(RustType.fromStandardType(primitive.standardType))
              case opt: Schema.Optional[_] =>
                boxIfNeeded(opt.schema).map: elementType =>
                  RustType.optional(elementType)
              case fail: Schema.Fail[_] =>
                ZPure.fail(fail.message)
              case tuple: Schema.Tuple2[_, _] =>
                for
                  left <- boxIfNeeded(tuple.left)
                  right <- boxIfNeeded(tuple.right)
                yield RustType.tuple2(left, right)
              case either: Schema.Either[_, _] =>
                for
                  left <- boxIfNeeded(either.left)
                  right <- boxIfNeeded(either.right)
                yield RustType.result(right, left)
              case lzy: Schema.Lazy[_] =>
                boxIfNeeded(lzy.schema)
              case _: Schema.Dynamic =>
                ZPure.succeed(RustType.json)
      end typeRef
      typeRef.flatMap: rustType =>
        updateState(s => s.copy(typeRefs = s.typeRefs.updated(schema, rustType))).as(rustType)

  private def ifNotProcessed[A](value: Schema[A])(f: => Fx[Unit]): Fx[Unit] =
    getState.flatMap: state =>
      if state.processed.contains(value) then ZPure.unit
      else updateState(_.copy(processed = state.processed + value)).zipRight(f)

  private def addDef(definition: RustDef): Fx[Unit] =
    updateState(state => state.copy(definitions = state.definitions :+ definition))

  private def addDefs(definitions: Chunk[RustDef]): Fx[Unit] =
    updateState(state => state.copy(definitions = state.definitions ++ definitions))

  private def addRequiredCrates(crates: Set[Crate]): Fx[Unit] =
    updateState(state => state.copy(requiredCrates = state.requiredCrates ++ crates))

  private def eval[A](schema: Schema[A]): Schema[A] =
    schema match
      case lzy: Schema.Lazy[_] => lzy.schema
      case _                   => schema

  private def canDeriveOrdFor(schema: Schema[?]): Fx[Boolean] =
    getCapsOrDefault(schema).map(_.ord)

  private def canDeriveHashFor(schema: Schema[?]): Fx[Boolean] =
    getCapsOrDefault(schema).map(_.hash)

  private def process[A](schema: Schema[A]): Fx[Unit] =
    ifNotProcessed(schema):
      getRustType(schema).flatMap: typeRef =>
        stacked(schema):
          schema match
            case sum: Schema.Enum[_] =>
              val rustCases = sum.cases.forEach: constructor =>
                val forced = eval(constructor.schema)
                forced match
                  case record: Schema.Record[_] if record.fields.isEmpty =>
                    // Simple enum case with no fields
                    ZPure.succeed(
                      RustDef.Newtype(Name.fromString(constructor.id), RustType.unit, derives = Chunk.empty)
                    )
                  case record: Schema.Record[_] =>
                    // Records with at least one field get inlined
                    val rustFields = record.fields.forEach: field =>
                      process(field.schema) *>
                        boxIfNeeded(field.schema).map: rustType =>
                          val name = Name.fromString(field.name)
                          val snakeName = name.toSnakeCase
                          RustDef.Field(
                            snakeName,
                            rustType,
                            if name.asString != snakeName.asString then Chunk(RustAttribute(RustType.serde, s"""rename = "${name.asString}""""))
                            else Chunk.empty,
                            isPublic = false
                          )

                    rustFields.map: fields =>
                      RustDef
                        .pubStruct(Name.fromString(record.id.name), fields: _*)
                  case _ =>
                    // Otherwise we generate the constructor type separately and just refer to it
                    process(constructor.schema) *>
                      getRustType(constructor.schema).map: rustType =>
                        RustDef.Newtype(Name.fromString(constructor.id), rustType, derives = Chunk.empty)

              for
                canBeOrd <- canDeriveOrdFor(sum)
                canBeHash <- canDeriveHashFor(sum)
                cases <- rustCases
                _ <- addDef(
                  RustDef
                    .`enum`(Name.fromString(sum.id.name), cases: _*)
                    .derive(RustType.debug)
                    .derive(RustType.rustClone)
                    .derive(RustType.partialEq)
                    .derive(RustType.eq)
                    .deriveIf(canBeHash)(RustType.hash)
                    .deriveIf(canBeOrd)(RustType.ord)
                    .deriveIf(canBeOrd)(RustType.partialOrd)
                    .derive(RustType.serialize)
                    .derive(RustType.deserialize)
                )
              yield ()

            case record: Schema.Record[_] =>
              val rustFields = record.fields.forEach: field =>
                process(field.schema) *>
                  boxIfNeeded(field.schema).map: rustType =>
                    val name = Name.fromString(field.name)
                    val snakeName = name.toSnakeCase
                    RustDef.Field(
                      snakeName,
                      rustType,
                      if name.asString != snakeName.asString then Chunk(RustAttribute(RustType.serde, s"""rename = "${name.asString}"""")) else Chunk.empty
                    )

              for
                canBeOrd <- canDeriveOrdFor(record)
                canBeHash <- canDeriveHashFor(record)

                fields <- rustFields
                _ <- addDef(
                  RustDef
                    .pubStruct(Name.fromString(record.id.name), fields: _*)
                    .derive(RustType.debug)
                    .derive(RustType.rustClone)
                    .derive(RustType.partialEq)
                    .derive(RustType.eq)
                    .deriveIf(canBeHash)(RustType.hash)
                    .deriveIf(canBeOrd)(RustType.ord)
                    .deriveIf(canBeOrd)(RustType.partialOrd)
                    .derive(RustType.serialize)
                    .derive(RustType.deserialize)
                )
              yield ()

            case map: Schema.Map[_, _] =>
              for
                _ <- process(map.keySchema)
                _ <- process(map.valueSchema)
              yield ()

            case seq: Schema.Sequence[_, _, _] =>
              process(seq.elementSchema)

            case set: Schema.Set[_] =>
              process(set.elementSchema)

            case transform: Schema.Transform[_, _, _] =>
              process(transform.schema)

            case primitive: Schema.Primitive[_] =>
              addDefs(Chunk.fromIterable(RustType.requiredDefs(primitive.standardType))) *>
                addRequiredCrates(RustType.requiredCrates(primitive.standardType))

            case opt: Schema.Optional[_] =>
              process(opt.schema)

            case fail: Schema.Fail[_] =>
              ZPure.fail(fail.message)
            case tuple: Schema.Tuple2[_, _] =>
              for
                left <- process(tuple.left)
                right <- process(tuple.right)
              yield ()

            case either: Schema.Either[_, _] =>
              for
                left <- process(either.left)
                right <- process(either.right)
              yield ()

            case lzy: Schema.Lazy[_] =>
              process(lzy.schema)
            case _: Schema.Dynamic =>
              addRequiredCrates(Set(Crate.serdeJson))
