package zio.http.rust

import zio.Chunk
import zio.rust.codegen.ast.{Crate, Name, RustDef}
import zio.schema.Schema

final case class RustEndpoints(name: Name, endpoints: Chunk[RustEndpoint]):
  def ++(endpoint: RustEndpoint): RustEndpoints = this.copy(endpoints = endpoints :+ endpoint)
  def ++(endpoints: RustEndpoints): RustEndpoints = this.copy(endpoints = this.endpoints ++ endpoints.endpoints)

  def definitions: Chunk[RustDef] =
    endpoints.flatMap(_.extraDefs).distinct

  def liveName: Name = name.toPascalCase + "Live"

  def named(name: String): RustEndpoints = copy(name = Name.fromString(name))

  def requiredCrates: Set[Crate] =
    endpoints.flatMap(_.requiredCrates).toSet + Crate.reqwest + Crate.asyncTrait