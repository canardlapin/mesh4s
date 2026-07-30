package mesh4s

/** Untrusted triangle rows at a codec or application boundary.
  *
  * This record deliberately admits malformed input. Only [[TriangleTopology.fromTable]]
  * establishes the manifold laws.
  */
final case class TriangleTable(
    vertexCount: Int,
    faces: Vector[Triangle[Int]]
)

object TriangleTable:
  def fromRows(
      vertexCount: Int,
      faces: IterableOnce[IterableOnce[Int]]
  ): Either[TriangleTableError, TriangleTable] =
    val output = Vector.newBuilder[Triangle[Int]]
    val iterator = faces.iterator
    var face = 0
    var error = Option.empty[TriangleTableError]
    while iterator.hasNext && error.isEmpty do
      val row = iterator.next().iterator.toVector
      if row.length == 3 then output += Triangle(row(0), row(1), row(2))
      else error = Some(TriangleTableError.WrongCornerCount(face, row.length))
      face += 1
    error.toLeft(TriangleTable(vertexCount, output.result()))

enum TriangleTableError derives CanEqual:
  case WrongCornerCount(face: Int, actual: Int)

  def message: String =
    this match
      case WrongCornerCount(face, actual) =>
        s"face $face has $actual corners; triangular input requires exactly 3"
