package sbt
package sbtabridge

/** The sbt 1 spelling of what [[SbtaBridge]] needs from sbt. */
private[sbtabridge] object Versioned {
  def extract(state: State): Extracted = Project.extract(state)

  /** A setting's value, or the task (or input task) a task key holds. */
  def lookup(extracted: Extracted, key: Def.ScopedKey[_]): Option[Any] =
    extracted.structure.data.get(key.scope, key.key)

  def toPath(extracted: Extracted): xsbti.VirtualFileRef => java.nio.file.Path = {
    val converter = extracted.get(Keys.fileConverter)
    ref => converter.toPath(ref)
  }
}
