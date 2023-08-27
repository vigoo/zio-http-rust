package zio.http.rust

import bleep.model
import bleep.model.CrossProjectName

package object scripts {
  // will publish these with dependencies
  def projectsToPublish(crossName: model.CrossProjectName): Boolean =
    crossName.name.value.startsWith("zio")

  val groupId = "io.github.vigoo"
}
