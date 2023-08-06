package zio.rust.codegen.ast

import zio.Chunk

enum RustDef:
  case TypeAlias(name: Name, typ: RustType, derives: Chunk[RustType])
  case Newtype(name: Name, typ: RustType, derives: Chunk[RustType])
  case Struct(name: Name, fields: Chunk[RustDef.Field], derives: Chunk[RustType])
  case Enum(name: Name, cases: Chunk[RustDef], derives: Chunk[RustType])

  def derive(tpe: RustType): RustDef = this match
    case TypeAlias(name, typ, derives) => TypeAlias(name, typ, derives :+ tpe)
    case Newtype(name, typ, derives)   => Newtype(name, typ, derives :+ tpe)
    case Struct(name, fields, derives) => Struct(name, fields, derives :+ tpe)
    case Enum(name, cases, derives)    => Enum(name, cases, derives :+ tpe)

object RustDef:
  final case class Field(name: Name, tpe: RustType, attributes: Chunk[RustAttribute] = Chunk.empty)

  def `enum`(name: Name, cases: RustDef*): RustDef =
    Enum(name, Chunk.fromIterable(cases), derives = Chunk.empty)

  def newtype(name: Name, typ: RustType): RustDef =
    Newtype(name, typ, derives = Chunk.empty)

  def struct(name: Name, fields: Field*): RustDef =
    Struct(name, Chunk.fromIterable(fields), derives = Chunk.empty)
