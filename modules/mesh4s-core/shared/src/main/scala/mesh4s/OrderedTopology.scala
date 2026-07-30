package mesh4s

final class Cycle[+A] private (
    val values: Vector[A]
):
  require(values.nonEmpty, "a cycle must be nonempty")

  def size: Int =
    values.length

  def map[B](f: A => B): Cycle[B] =
    Cycle.unsafe(values.map(f))

  override def equals(other: Any): Boolean =
    other match
      case that: Cycle[?] =>
        values.length == that.values.length &&
        rotationsEqual(values, that.values)
      case _ => false

  override def hashCode(): Int =
    values.iterator.map(_.##).sum

  override def toString: String =
    values.mkString("Cycle(", ", ", ")")

  private def rotationsEqual(
      left: Vector[?],
      right: Vector[?]
  ): Boolean =
    if left.isEmpty then true
    else
      var offset = 0
      var found = false
      while offset < left.length && !found do
        var index = 0
        var equal = true
        while index < left.length && equal do
          equal = left(index) == right((index + offset) % right.length)
          index += 1
        found = equal
        offset += 1
      found

object Cycle:
  def fromVector[A](values: Vector[A]): Option[Cycle[A]] =
    Option.when(values.nonEmpty)(new Cycle(values))

  private[mesh4s] def unsafe[A](values: Vector[A]): Cycle[A] =
    new Cycle(values)

final class Path[+A] private (
    val values: Vector[A]
):
  require(values.nonEmpty, "a path must be nonempty")

  def size: Int =
    values.length

  def map[B](f: A => B): Path[B] =
    Path.unsafe(values.map(f))

  override def equals(other: Any): Boolean =
    other match
      case that: Path[?] => values == that.values
      case _             => false

  override def hashCode(): Int =
    values.##

  override def toString: String =
    values.mkString("Path(", ", ", ")")

object Path:
  def fromVector[A](values: Vector[A]): Option[Path[A]] =
    Option.when(values.nonEmpty)(new Path(values))

  private[mesh4s] def unsafe[A](values: Vector[A]): Path[A] =
    new Path(values)

enum VertexFan[+H, +F, +V]:
  case Interior(
      halfedges: Cycle[H],
      faces: Cycle[F],
      neighbors: Cycle[V]
  )
  case Boundary(
      halfedges: Path[H],
      faces: Path[F],
      neighbors: Path[V]
  )
