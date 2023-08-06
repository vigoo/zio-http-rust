package zio.http.rust.printer

import zio.Chunk
import zio.http.rust.RustEndpoint
import zio.parser.*
import zio.rust.codegen.ast.{Name, RustType}

object RustClient:
  import zio.rust.codegen.printer.Rust.*

  def traitFn: Rust[RustEndpoint] =
    Printer.byValue: endpoint =>
      Printer.printString("fn") ~~ Printer.printString(endpoint.name.toSnakeCase.asString) ~ Printer.print('(') ~ parameterList(
        endpoint.allParameters
      ) ~ Printer.print(')') ~~ Printer.printString("->") ~~ typename(endpoint.resultType) ~ Printer.print(';')

  private def parameterList: Rust[Chunk[(Name, RustType)]] =
    (parameterName ~ Printer.print(':') ~~ typename).repeatWithSep0(Printer.printString(", "))

  private def parameterName: Rust[Name] =
    Printer.anyString.contramap(_.toSnakeCase.asString)
