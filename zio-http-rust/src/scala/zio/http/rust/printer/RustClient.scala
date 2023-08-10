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
        RustDef.struct(endpoints.liveName, RustDef.Field(Name.fromString("base_url"), reqwestUrl)).derive(RustType.rustClone).derive(RustType.debug)
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
        //         url.query_pairs_mut().append_pair(name, value);
        indent(2) ~ str("let result") ~~ ch('=') ~~ typename(reqwestClient) ~ dcolon ~ str("builder()") ~ newline ~
        indent(3) ~ str(".build()?") ~ newline ~
        indent(3) ~ ch('.') ~ str(endpoint.method.toLowerCase) ~ parentheses(str("url")) ~ newline ~
        indent(3) ~ str(".send()") ~ newline ~
        indent(3) ~ str(".await") ~ ch(';') ~ newline ~
        indent(2) ~ str("todo!()") ~ newline ~
        indent(2) ~ ch('}') ~ newline

  private def parameterList: Rust[Chunk[(Name, RustType)]] =
    str("&self") ~ comma ~ (parameterName ~ ch(':') ~~ typename).repeatWithSep0(comma)

  private def parameterName: Rust[Name] =
    Printer.anyString.contramap(_.toSnakeCase.asString)

  private def reqwestClient: RustType = RustType.module("reqwest").primitive("Client")
  private def reqwestUrl: RustType = RustType.module("reqwest").primitive("Url")
