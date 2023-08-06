package zio.http.rust

import zio.rust.codegen.ast.{Name, RustType}

enum RustParameter:
  case Static(name: Name, value: String)
  case Parameter(name: Name, tpe: RustType)
