package zio.rust.codegen.ast

import zio.Chunk

enum RustDef:
  case TypeAlias(name: Name, typ: RustType, derives: Chunk[RustType])
  case Newtype(name: Name, typ: RustType, derives: Chunk[RustType])
  case Struct(name: Name, fields: Chunk[RustDef.Field], derives: Chunk[RustType])
  case Enum(name: Name, cases: Chunk[RustDef], derives: Chunk[RustType])
  case ImplTrait(implemented: RustType, forType: RustType, functions: Chunk[RustDef])
  case Function(name: Name, parameters: Chunk[RustDef.Parameter], returnType: RustType, body: String)

  def derive(tpe: RustType): RustDef = this match
    case TypeAlias(name, typ, derives) => TypeAlias(name, typ, derives :+ tpe)
    case Newtype(name, typ, derives)   => Newtype(name, typ, derives :+ tpe)
    case Struct(name, fields, derives) => Struct(name, fields, derives :+ tpe)
    case Enum(name, cases, derives)    => Enum(name, cases, derives :+ tpe)
    case _                             => this

  def deriveIf(condition: => Boolean)(tpe: RustType): RustDef =
    if condition then derive(tpe) else this

object RustDef:
  final case class Field(name: Name, tpe: RustType, attributes: Chunk[RustAttribute] = Chunk.empty, isPublic: Boolean = true)

  enum ParameterModifier:
    case None
    case Ref
    case MutRef

  enum Parameter:
    case Named(mod: ParameterModifier, name: Name, tpe: RustType)
    case Self(mod: ParameterModifier)

  def `enum`(name: Name, cases: RustDef*): RustDef =
    Enum(name, Chunk.fromIterable(cases), derives = Chunk.empty)

  def fn(name: Name, parameters: Chunk[Parameter], result: RustType, body: String): RustDef =
    Function(name, parameters, result, body)

  def impl(impleneted: RustType, forType: RustType, functions: RustDef*): RustDef =
    ImplTrait(impleneted, forType, Chunk.fromIterable(functions))

  def newtype(name: Name, typ: RustType): RustDef =
    Newtype(name, typ, derives = Chunk.empty)

  def struct(name: Name, fields: Field*): RustDef =
    Struct(name, Chunk.fromIterable(fields), derives = Chunk.empty)
