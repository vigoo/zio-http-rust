package zio.http.rust.printer

import zio.Chunk
import zio.http.rust.*
import zio.parser.*
import zio.rust.codegen.ast.{Name, RustAttribute, RustDef, RustType}

object RustClient:

  import zio.rust.codegen.printer.Rust.*

  private def asyncTrait = RustAttribute(RustType.asyncTrait, "")

  def clientTrait: Rust[RustEndpoints] =
    Printer.byValue: endpoints =>
      attribute(asyncTrait) ~
        str("pub trait") ~~ name(endpoints.name.toPascalCase) ~~ ch('{') ~ newline ~
        traitFn.repeatWithSep0(ch('\n'))(endpoints.endpoints) ~ newline ~ ch('}')

  def traitFn: Rust[RustEndpoint] =
    Printer.byValue: endpoint =>
      indent ~ str("async fn") ~~ name(endpoint.name.toSnakeCase) ~ parentheses(parameterList(endpoint.allParameters)) ~~ str("->") ~~ typename(
        endpoint.resultType
      ) ~ ch(';')

  def liveClientImpl: Rust[RustEndpoints] =
    Printer.byValue: endpoints =>
      definition(
        RustDef.pubStruct(endpoints.liveName, RustDef.Field(Name.fromString("base_url"), reqwestUrl)).derive(RustType.rustClone).derive(RustType.debug)
      ) ~ newline ~ newline ~
        attribute(asyncTrait) ~
        str("impl") ~~ name(endpoints.name.toPascalCase) ~~ str("for") ~~ name(endpoints.liveName.toPascalCase) ~~ ch('{') ~ newline ~
        implFn.repeatWithSep0(ch('\n'))(endpoints.endpoints) ~
        ch('}') ~ newline

  def implFn: Rust[RustEndpoint] =
    Printer.byValue: endpoint =>
      indent ~ str("async fn") ~~ name(endpoint.name.toSnakeCase) ~ parentheses(parameterList(endpoint.allParameters)) ~~ str("->") ~~ typename(
        endpoint.resultType
      ) ~~ ch('{') ~ newline ~
        indent(2) ~ str("let mut url") ~~ ch('=') ~~ str("self.base_url.clone()") ~ ch(';') ~ newline ~
        indent(2) ~ str("url.set_path") ~ parentheses(str(endpoint.pathExpression)) ~ ch(';') ~ newline ~
        queryParameters(endpoint.queryParameters) ~ newline ~
        (if (endpoint.headers.nonEmpty) then
           indent(2) ~ str("let mut headers") ~~ ch('=') ~~ typename(reqwestHeaderMap) ~ dcolon ~ str("new();") ~ newline ~
             headers(endpoint.headers) ~ newline
         else Printer.unit ~ newline) ~
        indent(2) ~ str("let result") ~~ ch('=') ~~ typename(reqwestClient) ~ dcolon ~ str("builder()") ~ newline ~
        indent(3) ~ str(".build()?") ~ newline ~
        indent(3) ~ ch('.') ~ str(endpoint.method.toLowerCase) ~ parentheses(str("url")) ~ newline ~
        (if endpoint.headers.nonEmpty then indent(3) ~ str(".headers(headers)") ~ newline
         else Printer.unit) ~
        (if endpoint.bodies.size == 1 then indent(3) ~ str(".json") ~ parentheses(ch('&') ~ name(endpoint.bodies.head._1)) ~ newline
         else Printer.unit) ~
        indent(3) ~ str(".send()") ~ newline ~
        indent(3) ~ str(".await?") ~ ch(';') ~ newline ~
        indent(2) ~ str("match") ~~ str("result.status().as_u16()") ~~ ch('{') ~ newline ~
        indent(3) ~ str("200") ~~ str("=>") ~~ ch('{') ~ newline ~
        indent(4) ~ str("let body") ~~ ch('=') ~~ str("result.json::<") ~ typename(endpoint.successType) ~ str(">().await?") ~ ch(';') ~ newline ~
        indent(4) ~ str("Ok") ~ parentheses(str("body")) ~ newline ~
        indent(3) ~ ch('}') ~ newline ~
        errorDecoding.repeatWithSep0(newline)(Chunk.fromIterable(endpoint.errors.map((k, v) => (endpoint.errorType, k.code, v)))) ~ newline ~
        indent(3) ~ str("_") ~~ str("=>") ~~ str("Err") ~ parentheses(
          typename(endpoint.errorType) ~ dcolon ~ str("UnexpectedStatus") ~ parentheses(str("result.status()"))
        ) ~ newline ~
        indent(2) ~ ch('}') ~ newline ~
        indent(1) ~ ch('}') ~ newline

  private def queryParameters: Rust[Chunk[RustParameter]] =
    queryParameter.repeatWithSep0(newline)

  private def queryParameter: Rust[RustParameter] =
    Printer.byValue:
      case RustParameter.Static(n, value) =>
        indent(2) ~ str("url.query_pairs_mut().append_pair") ~ parentheses(ch('"') ~ name(n) ~ ch('"') ~ comma ~ ch('"') ~ str(value) ~ ch('"')) ~ ch(';')
      case RustParameter.Parameter(n, RustType.Option(tpe)) =>
        indent(2) ~ str("if let Some(value) = ") ~ name(n.toSnakeCase) ~ str(" {") ~ newline ~
          indent(3) ~ str("url.query_pairs_mut().append_pair") ~ parentheses(
            ch('"') ~ name(n) ~ ch('"') ~ comma ~ str("&format!(\"{value}\")")
          ) ~ ch(';') ~ newline ~
          indent(2) ~ ch('}')
      case RustParameter.Parameter(n, _) =>
        indent(2) ~ str("url.query_pairs_mut().append_pair") ~ parentheses(
          ch('"') ~ name(n) ~ ch('"') ~ comma ~ str("&format!(\"{") ~ name(n.toSnakeCase) ~ str("}\")")
        ) ~ ch(';')

  private def headers: Rust[Chunk[RustParameter]] =
    header.repeatWithSep0(newline)

  private def header: Rust[RustParameter] =
    Printer.byValue:
      case RustParameter.Static(n, value) =>
        indent(2) ~ str("headers.append") ~ parentheses(
          ch('"') ~ name(n) ~ ch('"') ~ comma ~ typename(reqwestHeaderValue) ~ dcolon ~ str("from_str") ~ parentheses(ch('"') ~ str(value) ~ ch('"')) ~ ch('?')
        ) ~ ch(';')
      case RustParameter.Parameter(n, RustType.Option(tpe)) =>
        indent(2) ~ str("if let Some(value) = ") ~ name(n.toSnakeCase) ~ str(" {") ~ newline ~
          indent(3) ~ str("headers.append") ~ parentheses(
            ch('"') ~ name(n) ~ ch('"') ~ comma ~
              typename(reqwestHeaderValue) ~ dcolon ~ str("from_str") ~ parentheses(str("&format!(\"{value}\")")) ~ ch('?')
          ) ~ ch(';') ~ newline ~
          indent(2) ~ ch('}')
      case RustParameter.Parameter(n, _) =>
        indent(2) ~ str("headers.append") ~ parentheses(
          ch('"') ~ name(n) ~ ch('"') ~ comma ~ typename(reqwestHeaderValue) ~ dcolon ~ str("from_str") ~ parentheses(
            str("&format!(\"{") ~ name(n.toSnakeCase) ~ str("}\")")
          ) ~ ch('?')
        ) ~ ch(';')

  private def errorDecoding: Rust[(RustType, Int, RustEndpoint.EndpointErrorCase)] =
    Printer.byValue:
      case (errorType, status, RustEndpoint.EndpointErrorCase.Simple(_)) =>
        indent(3) ~ str(status.toString) ~~ str("=>") ~~ str("Err") ~ parentheses(
          typename(errorType) ~ dcolon ~ str(s"Status${status}")
        ) ~ comma
      case (errorType, status, RustEndpoint.EndpointErrorCase.Inlined(fields, cname, errorAdtType, errorAdtName, _)) =>
        indent(3) ~ str(status.toString) ~~ str("=>") ~~ ch('{') ~ newline ~
          indent(4) ~ str("let body") ~~ ch('=') ~~ str("result.json::<") ~ name(errorAdtName ++ cname + "Payload") ~ str(">().await?") ~ ch(';') ~ newline ~
          indent(4) ~ str("Err") ~ parentheses(
            typename(errorType) ~ dcolon ~ str(s"Status${status}") ~~ ch('{') ~~ fieldsFromStruct(Name.fromString("body"))(fields) ~~ ch('}')
          ) ~ newline ~
          indent(3) ~ ch('}')
      case (errorType, status, RustEndpoint.EndpointErrorCase.ExternalType(tpe)) =>
        indent(3) ~ str(status.toString) ~~ str("=>") ~~ ch('{') ~ newline ~
          indent(4) ~ str("let body") ~~ ch('=') ~~ str("result.json::<") ~ typename(tpe) ~ str(">().await?") ~ ch(';') ~ newline ~
          indent(4) ~ str("Err") ~ parentheses(typename(errorType) ~ dcolon ~ str(s"Status${status}") ~ parentheses(str("body"))) ~ newline ~
          indent(3) ~ ch('}')

  def errorAdtConversion: Rust[(Name, Map[Int, RustEndpoint.EndpointErrorCase], RustType)] =
    Printer.byValue: (name, cases, errorType) =>
      str("match self") ~~ ch('{') ~ newline ~
        errorAdtConversionCase.*(Chunk.fromIterable(cases.map((k, v) => (k, (v, name, errorType))))) ~
        indent(3) ~ ch('_') ~~ str("=>") ~~ str("None") ~ newline ~
        indent(2) ~ ch('}')

  private def errorAdtConversionCase: Rust[(Int, (RustEndpoint.EndpointErrorCase, Name, RustType))] =
    Printer.byValue:
      case (status, (RustEndpoint.EndpointErrorCase.Simple(cname), n, errorType)) =>
        indent(3) ~ name(n) ~ dcolon ~ str(s"Status${status}") ~~ str("=>") ~~ str("Some") ~ parentheses(
          typename(errorType) ~ dcolon ~ name(cname)
        ) ~ comma ~ newline
      case (status, (RustEndpoint.EndpointErrorCase.Inlined(fields, cname, _, _, _), n, errorType)) =>
        indent(3) ~ name(n) ~ dcolon ~ str(s"Status${status}") ~~ ch('{') ~~ structFieldPatterns(fields) ~~ ch('}') ~~ str("=>") ~~
          str("Some") ~ parentheses(typename(errorType) ~ dcolon ~ name(cname) ~~ ch('{') ~~ clonedFields(fields) ~~ ch('}')) ~
          comma ~ newline
      case (status, (RustEndpoint.EndpointErrorCase.ExternalType(_), _, _)) => Printer.unit

  private def structFieldPatterns: Rust[Chunk[RustDef.Field]] =
    structFieldPattern.repeatWithSep0(comma)

  private def structFieldPattern: Rust[RustDef.Field] =
    Printer.byValue: field =>
      name(field.name)

  private def clonedFields: Rust[Chunk[RustDef.Field]] =
    clonedField.repeatWithSep0(comma)

  private def clonedField: Rust[RustDef.Field] =
    Printer.byValue: field =>
      name(field.name) ~ ch(':') ~~ name(field.name) ~ str(".clone()")

  private def fieldsFromStruct(n: Name): Rust[Chunk[RustDef.Field]] =
    fieldFromStruct(n).repeatWithSep0(comma)

  private def fieldFromStruct(n: Name): Rust[RustDef.Field] =
    Printer.byValue: field =>
      name(field.name) ~ ch(':') ~~ name(n) ~ ch('.') ~ name(field.name)

  private def parameterList: Rust[Chunk[(Name, RustType)]] =
    str("&self") ~ comma ~ (parameterName ~ ch(':') ~~ typename).repeatWithSep0(comma)

  private def parameterName: Rust[Name] =
    Printer.anyString.contramap(_.toSnakeCase.asString)

  private def reqwestClient: RustType = RustType.module("reqwest").primitive("Client")
  private def reqwestUrl: RustType = RustType.module("reqwest").primitive("Url")
  private def reqwestHeaderMap: RustType = RustType.module("reqwest", "header").primitive("HeaderMap")
  private def reqwestHeaderValue: RustType = RustType.module("reqwest", "header").primitive("HeaderValue")
