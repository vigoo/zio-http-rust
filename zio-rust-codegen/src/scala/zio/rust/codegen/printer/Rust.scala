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

  def bracketed[A](inner: Rust[A]): Rust[A] =
    lt ~ inner ~ gt

  def ch(ch: Char): Rust[Any] = Printer.print(ch)

  def comma: Rust[Any] = Printer.printString(", ")

  def dcolon: Rust[Any] = Printer.printString("::")

  def indent: Rust[Any] = Printer.printString("    ")

  def indent(n: Int): Rust[Any] = Printer.printString(" " * (n * 4))

  def gt: Rust[Any] = Printer.print('>')

  def lparen: Rust[Any] = Printer.print('(')

  def lt: Rust[Any] = Printer.print('<')

  def newline: Rust[Any] = Printer.print('\n')

  def parentheses[A](inner: Rust[A]): Rust[A] =
    lparen ~ inner ~ rparen

  def str(s: String): Rust[Any] = Printer.printString(s)

  def rparen: Rust[Any] = Printer.print(')')

  def typename: Rust[RustType] = Printer.byValue:
    case RustType.Primitive(name)             => str(name)
    case RustType.Option(inner)               => typename(RustType.Primitive("Option")) ~ bracketed(typename(inner))
    case RustType.Vec(inner)                  => typename(RustType.Primitive("Vec")) ~ bracketed(typename(inner))
    case RustType.SelectFromModule(path, typ) => Printer.anyString.repeatWithSep(dcolon)(path) ~ dcolon ~ typename(typ)
    case RustType.Parametric(name, params) =>
      str(name) ~ bracketed(typename.repeatWithSep(comma)(params))
    case RustType.Tuple(params) =>
      parentheses(typename.repeatWithSep(comma)(params))
    case RustType.Ref(inner)  => ch('&') ~ typename(inner)
    case RustType.Impl(inner) => str("impl") ~~ typename(inner)
    case RustType.Dyn(inner)  => str("dyn") ~~ typename(inner)
    case RustType.TypeConstrained(n, constraints) =>
      str(n) ~ bracketed((Printer.anyString ~~ ch('=') ~~ typename).repeatWithSep(comma)(constraints))
    case RustType.Plus(a, b) => typename(a) ~~ ch('+') ~~ typename(b)

  def definition: Rust[RustDef] = Printer.byValue:
    definition(0)(_)

  def definition(level: Int): Rust[RustDef] = Printer.byValue:
    case RustDef.TypeAlias(n, tpe, ds) =>
      indent(level) ~ derives(ds) ~ str("pub type") ~~ name(n) ~~ ch('=') ~~ typename(tpe) ~ ch(';')
    case RustDef.Newtype(n, tpe, ds) =>
      indent(level) ~ derives(ds) ~ str("pub struct") ~~ name(n) ~ parentheses(typename(tpe)) ~
        ch(';')
    case RustDef.Struct(n, fields, ds, isPublic) =>
      indent(level) ~ derives(ds) ~ (if isPublic then str("pub ") else Printer.unit) ~ str("struct") ~~ name(n) ~~ ch('{') ~ newline ~
        structFields(1)(fields) ~ ch('}')
    case RustDef.Enum(n, cases, ds) =>
      indent(level) ~ derives(ds) ~ str("pub enum") ~~ name(n) ~~ ch('{') ~ newline ~
        enumCases(cases) ~ ch('}')
    case RustDef.ImplTrait(implemented, forType, functions) =>
      indent(level) ~ str("impl") ~~ typename(implemented) ~~ str("for") ~~ typename(forType) ~~ ch('{') ~ newline ~
        definition(1).*(functions) ~ newline ~ ch('}')
    case RustDef.Impl(tpe, functions) =>
      indent(level) ~ str("impl") ~~ typename(tpe) ~~ ch('{') ~ newline ~
        definition(1).*(functions) ~ newline ~ ch('}')
    case RustDef.Function(n, parameters, result, body, isPublic) =>
      indent(level) ~ (if isPublic then str("pub ") else Printer.unit) ~ str("fn") ~~ name(n) ~ parentheses(parameterList(parameters)) ~~ str("->") ~~ typename(
        result
      ) ~~ ch('{') ~ newline
        ~ indent(level + 1) ~ str(body) ~ newline ~
        indent(level) ~ ch('}')

  private def parameterList: Rust[Chunk[RustDef.Parameter]] =
    parameter.repeatWithSep0(comma)

  private def parameter: Rust[RustDef.Parameter] =
    Printer.byValue:
      case RustDef.Parameter.Named(mod, n, tpe) =>
        name(n) ~ str(":") ~~ parameterMod(mod) ~ typename(tpe)
      case RustDef.Parameter.Self(mod) =>
        parameterMod(mod) ~ str("self")

  private def parameterMod: Rust[RustDef.ParameterModifier] =
    Printer.byValue:
      case RustDef.ParameterModifier.None   => Printer.unit
      case RustDef.ParameterModifier.Ref    => ch('&')
      case RustDef.ParameterModifier.MutRef => str("&mut ")

  private def parameterName: Rust[Name] =
    Printer.anyString.contramap(_.toSnakeCase.asString)

  private def derives: Rust[Chunk[RustType]] =
    Printer.byValue: types =>
      if types.isEmpty then Printer.unit
      else str("#[derive(") ~ typename.repeatWithSep(comma)(types) ~ str(")]\n")

  private def structFields(level: Int = 1): Rust[Chunk[RustDef.Field]] =
    structField(level).*

  private def structField(level: Int = 1): Rust[RustDef.Field] = Printer.byValue: field =>
    (indent(level) ~ attribute).*(field.attributes) ~
      indent(level) ~ (if field.isPublic then str("pub ") else Printer.unit) ~ name(field.name) ~ str(":") ~~ typename(field.tpe) ~ ch(',') ~ newline

  def name: Rust[Name] =
    Printer.anyString.contramap(_.asString)

  private def enumCases: Rust[Chunk[RustDef]] =
    enumCase.*

  private def enumCase: Rust[RustDef] = Printer.byValue:
    case RustDef.Newtype(n, RustType.unit, _) =>
      indent ~ name(n) ~ ch(',') ~ newline
    case RustDef.Newtype(n, tpe, _) =>
      indent ~ name(n) ~ ch('(') ~ typename(tpe) ~ ch(')') ~ ch(',') ~ Printer
        .print(
          '\n'
        )
    case RustDef.Struct(n, fields, _, _) =>
      indent ~ name(n) ~~ ch('{') ~ newline ~
        structFields(2)(fields) ~ indent ~ ch('}') ~ ch(',') ~ newline
    case _ =>
      Printer.fail("Only newtypes and structs are supported as enum cases")

  def attribute: Rust[RustAttribute] =
    Printer.byValue: attr =>
      if attr.body.nonEmpty then str("#[") ~ typename(attr.tpe) ~ parentheses(str(attr.body)) ~ ch(']') ~ newline
      else str("#[") ~ typename(attr.tpe) ~ ch(']') ~ newline
