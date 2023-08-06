package zio.rust.codegen.printer

import zio.Chunk
import zio.parser.*
import zio.parser.internal.PUnzippable
import zio.rust.codegen.ast.{Name, RustAttribute, RustDef, RustType}

import scala.annotation.targetName

object Rust:
  type Rust[-A] = Printer[String, Char, A]

  extension [Value](self: Rust[Value])
    @targetName("zipWithSpace")
    def ~~[Value2, Result](that: => Rust[Value2])(implicit zippable: PUnzippable.In[Value, Value2, Result]): Rust[Result] =
      self ~ Printer.char(' ') ~ that

  def typename: Rust[RustType] = Printer.byValue:
    case RustType.Primitive(name)             => Printer.printString(name)
    case RustType.Option(inner)               => typename(RustType.Primitive("Option")) ~ Printer.print('<') ~ typename(inner) ~ Printer.print('>')
    case RustType.Vec(inner)                  => typename(RustType.Primitive("Vec")) ~ Printer.print('<') ~ typename(inner) ~ Printer.print('>')
    case RustType.SelectFromModule(path, typ) => Printer.anyString.repeatWithSep(Printer.printString("::"))(path) ~ Printer.printString("::") ~ typename(typ)
    case RustType.Parametric(name, params) =>
      Printer.printString(name) ~ Printer.print('<') ~ typename.repeatWithSep(Printer.printString(", "))(params) ~ Printer.print('>')
    case RustType.Tuple(params) =>
      Printer.print('(') ~ typename.repeatWithSep(Printer.printString(", "))(params) ~ Printer.print(')')
    case RustType.Ref(inner) => Printer.print('&') ~ typename(inner)

  def definition: Rust[RustDef] = Printer.byValue:
    case RustDef.TypeAlias(name, tpe, ds) =>
      derives(ds) ~ Printer.printString("pub type") ~~ Printer.printString(name.asString) ~~ Printer.print('=') ~~ typename(tpe) ~ Printer.print(';')
    case RustDef.Newtype(name, tpe, ds) =>
      derives(ds) ~ Printer.printString("pub struct") ~~ Printer.printString(name.asString) ~ Printer.print('(') ~ typename(tpe) ~ Printer.print(')') ~
        Printer.print(';')
    case RustDef.Struct(name, fields, ds) =>
      derives(ds) ~ Printer.printString("pub struct") ~~ Printer.printString(name.asString) ~~ Printer.print('{') ~ Printer.print('\n') ~
        structFields(fields) ~ Printer.print('}')
    case RustDef.Enum(name, cases, ds) =>
      derives(ds) ~ Printer.printString("pub enum") ~~ Printer.printString(name.asString) ~~ Printer.print('{') ~ Printer.print('\n') ~
        enumCases(cases) ~ Printer.print('}')

  private def derives: Rust[Chunk[RustType]] =
    Printer.byValue: types =>
      if types.isEmpty then Printer.unit
      else Printer.printString("#[derive(") ~ typename.repeatWithSep(Printer.printString(", "))(types) ~ Printer.printString(")]\n")

  private def structFields: Rust[Chunk[RustDef.Field]] =
    structField.*

  private def structField: Rust[RustDef.Field] = Printer.byValue: field =>
    attribute.*(field.attributes) ~
      Printer.printString("    pub ") ~ name(field.name) ~ Printer.printString(":") ~~ typename(field.tpe) ~ Printer.print(',') ~ Printer.print('\n')

  private def name: Rust[Name] =
    Printer.anyString.contramap(_.asString)

  private def enumCases: Rust[Chunk[RustDef]] =
    enumCase.*

  private def enumCase: Rust[RustDef] = Printer.byValue:
    case RustDef.Newtype(name, RustType.unit, _) =>
      Printer.printString("    ") ~ Printer.printString(name.asString) ~ Printer.print(',') ~ Printer.print('\n')
    case RustDef.Newtype(name, tpe, _) =>
      Printer.printString("    ") ~ Printer.printString(name.asString) ~ Printer.print('(') ~ typename(tpe) ~ Printer.print(')') ~ Printer.print(',') ~ Printer
        .print(
          '\n'
        )
    case RustDef.Struct(name, fields, _) =>
      Printer.printString("    ") ~~ Printer.printString(name.asString) ~~ Printer.print('{') ~ Printer.print('\n') ~
        structFields(fields) ~ Printer.print('}') ~ Printer.print(',')
    case _ =>
      Printer.fail("Only newtypes and structs are supported as enum cases")

  private def attribute: Rust[RustAttribute] =
    Printer.byValue: attr =>
      Printer.printString("    #[") ~ typename(attr.tpe) ~ Printer.print('(') ~ Printer.printString(attr.body) ~ Printer.print(')') ~ Printer.print(
        ']'
      ) ~ Printer.print('\n')
