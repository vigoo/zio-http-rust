package zio.http.rust

import zio.rust.codegen.ast.RustType

object Types:
  val reqwestClient: RustType = RustType.module("reqwest").primitive("Client")

  val reqwestUrl: RustType = RustType.module("reqwest").primitive("Url")

  val reqwestHeaderMap: RustType = RustType.module("reqwest", "header").primitive("HeaderMap")

  val reqwestHeaderValue: RustType = RustType.module("reqwest", "header").primitive("HeaderValue")

  val reqwestMultipartForm: RustType = RustType.module("reqwest", "multipart").primitive("Form")

  val reqwestMultipartPart: RustType = RustType.module("reqwest", "multipart").primitive("Part")

  val reqwestBody: RustType = RustType.module("reqwest").primitive("Body")

  val intoBody: RustType = RustType.impl(RustType.parametric("Into", reqwestBody) + RustType.send)
