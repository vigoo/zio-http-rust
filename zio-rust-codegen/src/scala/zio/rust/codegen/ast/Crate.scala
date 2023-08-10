package zio.rust.codegen.ast

final case class Crate(name: String, version: String, features: Set[String] = Set.empty)

object Crate:
  val asyncTrait: Crate = Crate("async-trait", "0.1.72")
  val bigdecimal: Crate = Crate("bigdecimal", "0.4.1")
  val chrono: Crate = Crate("chrono", "0.4.26")
  val chronoTz: Crate = Crate("chrono-tz", "0.8.3")
  val json: Crate = Crate("json", "0.12.4")
  val numBigInt: Crate = Crate("num-bigint", "0.4.3")
  val reqwest: Crate = Crate("reqwest", "0.11.18", features = Set("gzip", "json", "multipart"))
  val serde: Crate = Crate("serde", "1.0.181", features = Set("derive"))
  val uuid: Crate = Crate("uuid", "1.4.1", features = Set("serde"))
