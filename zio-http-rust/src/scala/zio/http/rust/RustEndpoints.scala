package zio.http.rust

import zio.Chunk
import zio.rust.codegen.ast.Name

final case class RustEndpoints(name: Name, endpoints: Chunk[RustEndpoint]):
  def ++(endpoint: RustEndpoint): RustEndpoints = this.copy(endpoints = endpoints :+ endpoint)
  def ++(endpoints: RustEndpoints): RustEndpoints = this.copy(endpoints = this.endpoints ++ endpoints.endpoints)

  def named(name: String): RustEndpoints = copy(name = Name.fromString(name))
