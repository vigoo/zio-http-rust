package zio.http.rust

import zio.http.rust.printer.RustClient
import zio.nio.file.{Files, Path}
import zio.rust.codegen.ast.{Crate, RustDef}
import zio.rust.codegen.printer.{Cargo, Rust}
import zio.schema.rust.RustModel
import zio.{Chunk, ZIO}

import java.io.IOException
import java.nio.charset.StandardCharsets

final case class ClientCrateGenerator(name: String, version: String, description: String, homepage: String, endpoints: Chunk[RustEndpoints]):
  private val allSchemas = endpoints.map(_.endpoints.toSet.flatMap(_.referredSchemas)).reduce(_ union _)

  def generate(targetDirectory: Path): ZIO[Any, Throwable, Unit] =
    for
      clientModel <- ZIO.fromEither(RustModel.fromSchemas(allSchemas.toSeq)).mapError(err => new RuntimeException(s"Failed to generate client model: $err"))
      cargoFile = targetDirectory / "Cargo.toml"
      srcDir = targetDirectory / "src"
      libFile = srcDir / "lib.rs"
      modelFile = srcDir / "model.rs"

      requiredCrates = clientModel.requiredCrates union endpoints.map(_.requiredCrates).reduce(_ union _)

      _ <- Files.createDirectories(targetDirectory)
      _ <- Files.createDirectories(srcDir)
      _ <- writeCargo(cargoFile, requiredCrates)
      _ <- writeLib(libFile)
      _ <- writeModel(modelFile, clientModel.definitions)
      _ <- ZIO.foreachDiscard(endpoints): endpoints =>
        val clientFile = srcDir / s"${endpoints.name.toSnakeCase}.rs"
        writeClient(clientFile, endpoints)
    yield ()

  private def writeCargo(path: Path, dependencies: Set[Crate]): ZIO[Any, Throwable, Unit] =
    for
      dependencyLines <- ZIO
        .foreach(dependencies)(crate => ZIO.fromEither(Cargo.crateDependency.printString(crate)))
        .mapError(e => new RuntimeException(s"Failed to print cargo file: $e"))
      content =
        s"""[package]
           |name = "$name"
           |version = "$version"
           |edition = "2021"
           |description = "$description"
           |license = "Apache-2.0"
           |homepage = "$homepage"
           |readme = false
           |
           |[lib]
           |
           |[dependencies]
           |${dependencyLines.mkString("\n")}
           |""".stripMargin
      _ <- Files.writeBytes(path, Chunk.fromArray(content.getBytes(StandardCharsets.UTF_8)))
    yield ()

  private def writeLib(path: Path): ZIO[Any, IOException, Unit] =
    val moduleNames = endpoints.map(_.name.toSnakeCase.asString) :+ "model"
    val lib = moduleNames.map(name => s"pub mod $name;").mkString("\n")

    Files.writeBytes(path, Chunk.fromArray(lib.getBytes(StandardCharsets.UTF_8)))

  private def writeModel(path: Path, defs: Chunk[RustDef]): ZIO[Any, Throwable, Unit] =
    for
      lines <- ZIO
        .foreach(defs)(defn => ZIO.fromEither(Rust.definition.printString(defn)))
        .mapError(e => new RuntimeException(s"Failed to print model file: $e"))
      content = lines.mkString("\n\n")
      _ <- Files.writeBytes(path, Chunk.fromArray(content.getBytes(StandardCharsets.UTF_8)))
    yield ()

  private def writeClient(path: Path, endpoints: RustEndpoints): ZIO[Any, Throwable, Unit] =
    for
      lines <- clientLines(endpoints)
        .mapError(e => new RuntimeException(s"Failed to print client file: $e"))
      content = lines.mkString("\n\n")
      _ <- Files.writeBytes(path, Chunk.fromArray(content.getBytes(StandardCharsets.UTF_8)))
    yield ()

  private def clientLines(endpoints: RustEndpoints): ZIO[Any, String, Chunk[String]] =
    for
      lines1 <- ZIO
        .foreach(endpoints.definitions)(defn => ZIO.fromEither(Rust.definition.printString(defn)))
      lines2 <- ZIO
        .fromEither(RustClient.clientTrait.printString(endpoints))
      lines3 <- ZIO
        .fromEither(RustClient.liveClientImpl.printString(endpoints))
    yield lines1 :+ lines2 :+ lines3
