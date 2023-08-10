package zio.schema.rust

import zio.Chunk
import zio.prelude.*
import zio.prelude.fx.*
import zio.rust.codegen.ast.{Crate, Name, RustAttribute, RustDef, RustType}
import zio.schema.Schema

final case class RustModel(typeRefs: Map[Schema[?], RustType], definitions: Chunk[RustDef], requiredCrates: Set[Crate])

object RustModel:
  final case class State(
      typeRefs: Map[Schema[?], RustType],
      definitions: Chunk[RustDef],
      requiredCrates: Set[Crate],
      processed: Set[Schema[?]],
      stack: Chunk[Schema[?]]
  )

  type Fx[+A] = ZPure[Nothing, State, State, Any, String, A]

  val empty: State = State(typeRefs = Map.empty, definitions = Chunk.empty, requiredCrates = Set.empty, processed = Set.empty, stack = Chunk.empty)

  def fromSchema[A](schema: Schema[A]): Either[String, RustModel] =
    process(schema).zipRight(ZPure.get[State]).provideState(empty).runEither.map(postProcess)

  def fromSchemas(schemas: Seq[Schema[?]]): Either[String, RustModel] =
    ZPure.forEach(schemas)(process).zipRight(ZPure.get[State]).provideState(empty).runEither.map(postProcess)

  private def postProcess(model: State): RustModel =
    RustModel(
      typeRefs = model.typeRefs,
      definitions = model.definitions.distinct,
      requiredCrates = model.requiredCrates + Crate.serde
    )

  private def getState: Fx[State] = ZPure.get[State]
  private def updateState(f: State => State): Fx[Unit] = ZPure.update[State, State](f)

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

  private def process[A](schema: Schema[A]): Fx[Unit] =
    ifNotProcessed(schema):
      getRustType(schema).flatMap: typeRef =>
        stacked(schema):
          schema match
            case sum: Schema.Enum[_] =>
              val rustCases = sum.cases.forEach: constructor =>
                process(constructor.schema) *>
                  getRustType(constructor.schema).map: rustType =>
                    Name.fromString(constructor.id) -> rustType

              rustCases.flatMap: cases =>
                addDef(
                  RustDef
                    .`enum`(Name.fromString(sum.id.name), cases.map((name, caseType) => RustDef.Newtype(name, caseType, derives = Chunk.empty)): _*)
                    .derive(RustType.debug)
                    .derive(RustType.rustClone)
                    .derive(RustType.serialize)
                    .derive(RustType.deserialize)
                )

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

              rustFields.flatMap: fields =>
                addDef(
                  RustDef
                    .struct(Name.fromString(record.id.name), fields: _*)
                    .derive(RustType.debug)
                    .derive(RustType.rustClone)
                    .derive(RustType.serialize)
                    .derive(RustType.deserialize)
                )

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
              addRequiredCrates(Set(Crate.json))
