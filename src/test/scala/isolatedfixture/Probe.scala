package isolatedfixture

class Probe:
  def ping: String = "pong"
  def each[A](f: A => Unit)(using codec: Codec[A]): (A, Int) = ???

/** Members of an object: what `inspect` must find besides top-level classes. */
object Registry:
  opaque type Version = String
  object Version:
    def parse(s: String): Version = s
    extension (v: Version) def value: String = v

  type Id = Long
  val default: Version = Version.parse("1")

  object Nested:
    def answer: Int = 42

/** A class with givens in its companion, as `DbCodec` has its built-in codecs. */
trait Codec[A]:
  def read(s: String): A

object Codec:
  given intCodec: Codec[Int] = _.toInt
  given listCodec[A](using a: Codec[A]): Codec[List[A]] = s => s.split(',').toList.map(a.read)
