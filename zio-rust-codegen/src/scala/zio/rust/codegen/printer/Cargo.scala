package zio.rust.codegen.printer

import zio.Chunk
import zio.parser.*
import zio.rust.codegen.ast.Crate

object Cargo:
  type Cargo[-A] = Printer[String, Char, A]

  def crateDependency: Cargo[Crate] = Printer.byValue: crate =>
    if crate.features.isEmpty then
      Printer.printString(crate.name) ~ Printer.printString(" = ") ~ Printer.print('"') ~ Printer.printString(crate.version) ~ Printer.print('"')
    else
      Printer.printString(crate.name) ~ Printer.printString(" = { version = ") ~
        Printer.print('"') ~ Printer.printString(crate.version) ~ Printer.print('"') ~ Printer.printString(", features = [") ~
        feature.repeatWithSep(Printer.printString(", "))(Chunk.fromIterable(crate.features)) ~
        Printer.printString("] }")

  private def feature: Cargo[String] =
    Printer.print('\"') ~ Printer.anyString ~ Printer.print('\"')
