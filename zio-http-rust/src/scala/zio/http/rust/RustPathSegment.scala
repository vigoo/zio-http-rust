package zio.http.rust

import zio.rust.codegen.ast.{Name, RustType}

enum RustPathSegment:
  case Literal(value: String)
  case Parameter(name: Name, tpe: RustType)
  case Trailing

  def isLiteral: Boolean =
    this match
      case Literal(_) => true
      case _          => false
