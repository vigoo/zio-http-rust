package zio.http.rust

import zio.Chunk
import zio.rust.codegen.ast.{Crate, Name, RustDef, RustType}
import zio.schema.Schema

final case class RustEndpoints(name: Name, originalEndpoints: Chunk[RustEndpoint]):
  def ++(endpoint: RustEndpoint): RustEndpoints = this.copy(originalEndpoints = originalEndpoints :+ endpoint)
  def ++(endpoints: RustEndpoints): RustEndpoints = this.copy(originalEndpoints = this.originalEndpoints ++ endpoints.originalEndpoints)

  lazy val endpoints: Chunk[RustEndpoint] =
    val commonErrors = originalEndpoints.map(_.errors).distinct.size == 1
    if commonErrors then
      val commonErrorName = name.toPascalCase + "Error"
      originalEndpoints.zipWithIndex
        .map: (endpoint, idx) =>
          endpoint.copy(
            unifiedErrorTypeName = Some(commonErrorName),
            generateUnifiedErrorType = idx == 0
          )
    else originalEndpoints

  def definitions: Chunk[RustDef] =
    endpoints.flatMap(_.extraDefs).distinct

  def liveName: Name = name.toPascalCase + "Live"

  def named(name: String): RustEndpoints = copy(name = Name.fromString(name))

  def requiredCrates: Set[Crate] =
    endpoints.flatMap(_.requiredCrates).toSet + Crate.reqwest + Crate.asyncTrait
