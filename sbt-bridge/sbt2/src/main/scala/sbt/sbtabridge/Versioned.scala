package sbt
package sbtabridge

/** The sbt 2 spelling of what [[SbtaBridge]] needs from sbt. */
private[sbtabridge] object Versioned {
  def extract(state: State): Extracted = {
    import sbt.ProjectExtra.extract
    Project.extract(state)
  }

  /** A setting's value, or the task (or input task) a task key holds. */
  def lookup(extracted: Extracted, key: Def.ScopedKey[?]): Option[Any] =
    extracted.structure.data.get(key)

  def toPath(extracted: Extracted): xsbti.VirtualFileRef => java.nio.file.Path = {
    val converter = extracted.get(Keys.fileConverter)
    ref => converter.toPath(ref)
  }
}
