package mesh4s.reference

/** Deliberately allocation-heavy formulas independent of packed geometry kernels. */
object ReferenceGeometry:
  final case class Barycentric(
      weights: Vector[Double],
      offPlaneResidual: Double
  )

  def edgeLength(
      points: Vector[Vector[Double]],
      first: Int,
      second: Int
  ): Double =
    math.sqrt(
      points(first)
        .zip(points(second))
        .map((left, right) => (right - left) * (right - left))
        .sum
    )

  def centroid(
      points: Vector[Vector[Double]],
      face: Vector[Int]
  ): Vector[Double] =
    points.head.indices
      .map: axis =>
        face.map(vertex => points(vertex)(axis)).sum / face.length.toDouble
      .toVector

  def unsignedArea(
      points: Vector[Vector[Double]],
      face: Vector[Int]
  ): Double =
    val first = subtract(points(face(1)), points(face(0)))
    val second = subtract(points(face(2)), points(face(0)))
    val determinant =
      dot(first, first) * dot(second, second) - dot(first, second) * dot(
        first,
        second
      )
    0.5 * math.sqrt(math.max(0.0, determinant))

  def signedArea2(
      points: Vector[Vector[Double]],
      face: Vector[Int]
  ): Double =
    val first = subtract(points(face(1)), points(face(0)))
    val second = subtract(points(face(2)), points(face(0)))
    0.5 * (first(0) * second(1) - first(1) * second(0))

  def vectorArea3(
      points: Vector[Vector[Double]],
      face: Vector[Int]
  ): Vector[Double] =
    val first = subtract(points(face(1)), points(face(0)))
    val second = subtract(points(face(2)), points(face(0)))
    Vector(
      0.5 * (first(1) * second(2) - first(2) * second(1)),
      0.5 * (first(2) * second(0) - first(0) * second(2)),
      0.5 * (first(0) * second(1) - first(1) * second(0))
    )

  /** Orthogonally project to the face plane and report residual magnitude. */
  def barycentric(
      points: Vector[Vector[Double]],
      face: Vector[Int],
      query: Vector[Double]
  ): Barycentric =
    val origin = points(face(0))
    val first = subtract(points(face(1)), origin)
    val second = subtract(points(face(2)), origin)
    val queryVector = subtract(query, origin)
    val firstFirst = dot(first, first)
    val firstSecond = dot(first, second)
    val secondSecond = dot(second, second)
    val queryFirst = dot(queryVector, first)
    val querySecond = dot(queryVector, second)
    val determinant = firstFirst * secondSecond - firstSecond * firstSecond
    val secondWeight =
      (queryFirst * secondSecond - querySecond * firstSecond) / determinant
    val thirdWeight =
      (querySecond * firstFirst - queryFirst * firstSecond) / determinant
    val firstWeight = 1.0 - secondWeight - thirdWeight
    val residual =
      queryVector.indices.map: axis =>
        queryVector(axis) -
          secondWeight * first(axis) -
          thirdWeight * second(axis)
    Barycentric(
      Vector(firstWeight, secondWeight, thirdWeight),
      math.sqrt(dot(residual.toVector, residual.toVector))
    )

  private def subtract(
      left: Vector[Double],
      right: Vector[Double]
  ): Vector[Double] =
    left.zip(right).map((first, second) => first - second)

  private def dot(left: Vector[Double], right: Vector[Double]): Double =
    left.zip(right).map(_ * _).sum
