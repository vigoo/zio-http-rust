package zio.http.rust.scripts

import bleep.*
import bleep.nosbt.InteractionService
import bleep.packaging.{CoordinatesFor, PackagedLibrary, PublishLayout, packageLibraries}
import bleep.plugin.cirelease.CiReleasePlugin
import bleep.plugin.dynver.DynVerPlugin
import bleep.plugin.pgp.PgpPlugin
import bleep.plugin.sonatype.Sonatype
import coursier.Info

import scala.collection.immutable.SortedMap

object Publish extends BleepScript("Publish") {
  def run(started: Started, commands: Commands, args: List[String]): Unit = {
    commands.compile(started.build.explodedProjects.keys.filter(projectsToPublish).toList)

    val dynVer = new DynVerPlugin(baseDirectory = started.buildPaths.buildDir.toFile, dynverSonatypeSnapshots = true)
    val pgp = new PgpPlugin(
      logger = started.logger,
      maybeCredentials = None,
      interactionService = InteractionService.DoesNotMaskYourPasswordExclamationOneOne
    )
    val sonatype = new Sonatype(
      logger = started.logger,
      sonatypeBundleDirectory = started.buildPaths.dotBleepDir / "sonatype-bundle",
      sonatypeProfileName = groupId,
      bundleName = "zio-http-rust",
      version = dynVer.version,
      sonatypeCredentialHost = Sonatype.sonatypeLegacy
    )
    val ciRelease = new CiReleasePlugin(started.logger, sonatype, dynVer, pgp)

    started.logger.info(dynVer.version)

    val info = Info(
      "Generate Rust clients for zio-http servers",
      "https://github.com/vigoo/zio-http-rust/",
      List(
        Info.Developer(
          "vigoo",
          "Daniel Vigovszky",
          "https://github.com/vigoo"
        )
      ),
      publication = None,
      scm = CiReleasePlugin.inferScmInfo,
      licenseInfo = List(
        Info.License(
          "Apache License 2.0",
          Some("http://opensource.org/licenses/apache-2.0"),
          distribution = Some("repo"),
          comments = None
        )
      )
    )

    val packagedLibraries: SortedMap[model.CrossProjectName, PackagedLibrary] =
      packageLibraries(
        started,
        coordinatesFor = CoordinatesFor.Default(groupId = groupId, version = dynVer.version),
        shouldInclude = projectsToPublish,
        publishLayout = PublishLayout.Maven(info)
      )

    val files: Map[RelPath, Array[Byte]] =
      packagedLibraries.flatMap { case (_, PackagedLibrary(_, files)) => files.all }

    files.foreach { case (path, bytes) =>
      started.logger.withContext(path)(_.asString).withContext(bytes.length).debug("will publish")
    }
    ciRelease.ciRelease(files)
  }
}
