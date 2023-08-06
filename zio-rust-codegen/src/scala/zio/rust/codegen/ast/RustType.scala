package zio.rust.codegen.ast

import zio.Chunk
import zio.rust.codegen.ast.{Name, RustDef}
import zio.schema.StandardType

enum RustType:
  case Primitive(name: String)
  case Option(inner: RustType)
  case Vec(inner: RustType)
  case SelectFromModule(modulePath: Chunk[String], typ: RustType)
  case Parametric(name: String, args: Chunk[RustType])
  case Tuple(args: Chunk[RustType])
  case Ref(inner: RustType)

object RustType:
  val u8: RustType = Primitive("u8")
  val u16: RustType = Primitive("u16")
  val u32: RustType = Primitive("u32")
  val u64: RustType = Primitive("u64")
  val i8: RustType = Primitive("i8")
  val i16: RustType = Primitive("i16")
  val i32: RustType = Primitive("i32")
  val i64: RustType = Primitive("i64")
  val f32: RustType = Primitive("f32")
  val f64: RustType = Primitive("f64")
  val bool: RustType = Primitive("bool")
  val char: RustType = Primitive("char")
  val str: RustType = Primitive("str")
  val string: RustType = Primitive("String")
  val unit: RustType = Primitive("()")

  val rustClone: RustType = primitive("Clone")
  val debug: RustType = primitive("Debug")
  val json: RustType = module("json").primitive("JsonValue")
  val month: RustType = module("chrono").primitive("Month")
  val year: RustType = crate().primitive("Year")
  val uuid: RustType = module("uuid").primitive("Uuid")

  val serde: RustType = primitive("serde")
  val serialize: RustType = module("serde").primitive("Serialize")
  val deserialize: RustType = module("serde").primitive("Deserialize")

  def box(inner: RustType) = parametric("Box", inner)

  def crate(): RustTypeInModule = RustTypeInModule(Chunk("crate"))

  def fromStandardType(standardType: StandardType[?]): RustType =
    standardType match
      case StandardType.UnitType           => unit
      case StandardType.StringType         => string
      case StandardType.BoolType           => bool
      case StandardType.ByteType           => u8
      case StandardType.ShortType          => i16
      case StandardType.IntType            => i32
      case StandardType.LongType           => i64
      case StandardType.FloatType          => f32
      case StandardType.DoubleType         => f64
      case StandardType.BinaryType         => vec(u8)
      case StandardType.CharType           => char
      case StandardType.UUIDType           => uuid
      case StandardType.BigDecimalType     => module("bigdecimal").primitive("BigDecimal")
      case StandardType.BigIntegerType     => module("num_bigint").primitive("BigInt")
      case StandardType.DayOfWeekType      => module("chrono").primitive("Weekday")
      case StandardType.MonthType          => month
      case StandardType.MonthDayType       => crate().primitive("MonthDay")
      case StandardType.PeriodType         => module("std", "time").primitive("Duration")
      case StandardType.YearType           => year
      case StandardType.YearMonthType      => crate().primitive("YearMonth")
      case StandardType.ZoneIdType         => module("chrono").primitive("TimeZone")
      case StandardType.ZoneOffsetType     => module("chrono").primitive("FixedOffset")
      case StandardType.DurationType       => module("std", "time").primitive("Duration")
      case StandardType.InstantType        => module("std", "time").primitive("Instant")
      case StandardType.LocalDateType      => module("chrono", "naive").primitive("NaiveDate")
      case StandardType.LocalTimeType      => module("chrono", "naive").primitive("NaiveTime")
      case StandardType.LocalDateTimeType  => module("chrono").parametric("DateTime", module("chrono").primitive("Local"))
      case StandardType.OffsetTimeType     => module("chrono", "naive").primitive("NaiveTime")
      case StandardType.OffsetDateTimeType => module("chrono").parametric("DateTime", module("chrono").primitive("FixedOffset"))
      case StandardType.ZonedDateTimeType  => module("chrono").parametric("DateTime", module("chrono").primitive("FixedOffset"))

  def hashMap(keyType: RustType, valueType: RustType): RustType =
    module("std", "collections").parametric("HashMap", keyType, valueType)

  def hashSet(elemType: RustType): RustType =
    module("std", "collections").parametric("HashSet", elemType)

  def module(path: String*): RustTypeInModule = RustTypeInModule(Chunk.fromIterable(path))

  def optional(inner: RustType): RustType =
    RustType.Option(inner)

  def parametric(name: String, args: RustType*): RustType =
    RustType.Parametric(name, Chunk.fromIterable(args))

  def primitive(name: String): RustType =
    RustType.Primitive(name)

  def ref(inner: RustType): RustType =
    RustType.Ref(inner)
  def requiredCrates(standardType: StandardType[?]): Set[Crate] =
    standardType match
      case StandardType.UnitType           => Set.empty
      case StandardType.StringType         => Set.empty
      case StandardType.BoolType           => Set.empty
      case StandardType.ByteType           => Set.empty
      case StandardType.ShortType          => Set.empty
      case StandardType.IntType            => Set.empty
      case StandardType.LongType           => Set.empty
      case StandardType.FloatType          => Set.empty
      case StandardType.DoubleType         => Set.empty
      case StandardType.BinaryType         => Set.empty
      case StandardType.CharType           => Set.empty
      case StandardType.UUIDType           => Set(Crate.uuid)
      case StandardType.BigDecimalType     => Set(Crate.bigdecimal)
      case StandardType.BigIntegerType     => Set(Crate.numBigInt)
      case StandardType.DayOfWeekType      => Set(Crate.chrono)
      case StandardType.MonthType          => Set(Crate.chrono)
      case StandardType.MonthDayType       => Set.empty
      case StandardType.PeriodType         => Set.empty
      case StandardType.YearType           => Set.empty
      case StandardType.YearMonthType      => Set.empty
      case StandardType.ZoneIdType         => Set(Crate.chrono, Crate.chronoTz)
      case StandardType.ZoneOffsetType     => Set(Crate.chrono)
      case StandardType.DurationType       => Set.empty
      case StandardType.InstantType        => Set.empty
      case StandardType.LocalDateType      => Set(Crate.chrono)
      case StandardType.LocalTimeType      => Set(Crate.chrono)
      case StandardType.LocalDateTimeType  => Set(Crate.chrono)
      case StandardType.OffsetTimeType     => Set(Crate.chrono)
      case StandardType.OffsetDateTimeType => Set(Crate.chrono)
      case StandardType.ZonedDateTimeType  => Set(Crate.chrono)

  def requiredDefs(standardType: StandardType[?]): scala.Option[RustDef] =
    standardType match
      case StandardType.UnitType       => None
      case StandardType.StringType     => None
      case StandardType.BoolType       => None
      case StandardType.ByteType       => None
      case StandardType.ShortType      => None
      case StandardType.IntType        => None
      case StandardType.LongType       => None
      case StandardType.FloatType      => None
      case StandardType.DoubleType     => None
      case StandardType.BinaryType     => None
      case StandardType.CharType       => None
      case StandardType.UUIDType       => None
      case StandardType.BigDecimalType => None
      case StandardType.BigIntegerType => None
      case StandardType.DayOfWeekType  => None
      case StandardType.MonthType      => None
      case StandardType.MonthDayType   => Some(RustDef.newtype(Name.fromString("MonthDay"), u32))
      case StandardType.PeriodType     => None
      case StandardType.YearType       => Some(RustDef.newtype(Name.fromString("Year"), i32))
      case StandardType.YearMonthType =>
        Some(RustDef.struct(Name.fromString("YearMonth"), RustDef.Field(Name.fromString("year"), year), RustDef.Field(Name.fromString("month"), month)))
      case StandardType.ZoneIdType         => None
      case StandardType.ZoneOffsetType     => None
      case StandardType.DurationType       => None
      case StandardType.InstantType        => None
      case StandardType.LocalDateType      => None
      case StandardType.LocalTimeType      => None
      case StandardType.LocalDateTimeType  => None
      case StandardType.OffsetTimeType     => None
      case StandardType.OffsetDateTimeType => None
      case StandardType.ZonedDateTimeType  => None

  def result(ok: RustType, err: RustType): RustType =
    RustType.Parametric("Result", Chunk(ok, err))

  def tuple2(left: RustType, right: RustType): RustType =
    RustType.Tuple(Chunk(left, right))

  def vec(inner: RustType): RustType =
    RustType.Vec(inner)

final case class RustTypeInModule(path: Chunk[String]):
  def parametric(name: String, args: RustType*): RustType =
    RustType.Parametric(name, Chunk.fromIterable(args))

  def primitive(name: String): RustType =
    RustType.SelectFromModule(path, RustType.primitive(name))
