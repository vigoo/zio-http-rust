package zio.rust.codegen.ast

import zio.Chunk

opaque type Name = String
object Name:
  def fromString(name: String): Name = name

  extension (name: Name)
    def +(postfix: String): Name = Name.fromString(name + postfix)
    def ++(postfix: Name): Name = Name.fromString(name + postfix.asString)

    def asString: String = name

    def parts: Chunk[String] =
      Chunk.fromArray(
        name
          .split("(?=[A-Z\\-_])")
          .map(_.toLowerCase.stripPrefix("-").stripPrefix("_"))
      )

    def toKebabCase: Name = parts.mkString("-")
    def toSnakeCase: Name = parts.mkString("_")
    def toPascalCase: Name = parts
      .map(_.capitalize)
      .mkString
      .replace("-", "")
      .replace("_", "")
    def toCamelCase: Name = {
      val s = toPascalCase
      if (s.isEmpty) s
      else s.updated(0, s(0).toLower)
    }
