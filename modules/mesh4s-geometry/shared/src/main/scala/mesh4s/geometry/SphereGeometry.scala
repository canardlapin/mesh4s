package mesh4s.geometry

import spatial4s.D3
import spatial4s.Frame
import locus4s.Index

enum SphereGeometryError derives CanEqual:
  case InvalidRadius(value: Double)
  case InvalidTolerance(value: Double)
  case NotSpherical
  case NonFiniteProjection(vertex: Int, axis: Int)
  case ZeroRadius(vertex: Int)
  case NonFiniteRadius(vertex: Int, value: Double)

  def message: String = this match
    case InvalidRadius(value)    => s"sphere radius must be positive and finite, got $value"
    case InvalidTolerance(value) =>
      s"sphere tolerance must be finite and greater than one, got $value"
    case NotSpherical => "point field is not approximately spherical about the origin"
    case NonFiniteProjection(vertex, axis) =>
      s"non-finite spherical projection at vertex $vertex axis $axis"
    case ZeroRadius(vertex) =>
      s"spherical angular distance requires a non-zero radius at vertex $vertex"
    case NonFiniteRadius(vertex, value) =>
      s"spherical angular distance requires a finite radius at vertex $vertex, got $value"

/** Origin-centered geometry with explicit radius and admission tolerance. Template,
  * registration, coordinate-unit, and hemisphere policy belong to callers.
  */
object SphereGeometry:
  def radius[S, F <: Frame[D3]](
      points: PointField[S, D3, F],
      vertex: locus4s.Index[S]
  ): Double =
    norm(points, vertex.ordinal)

  def angularDistance[S, F <: Frame[D3]](
      points: PointField[S, D3, F],
      first: Index[S],
      second: Index[S]
  ): Either[SphereGeometryError, Double] =
    for
      firstRadius <- angularRadius(points, first.ordinal)
      secondRadius <- angularRadius(points, second.ordinal)
    yield
      if first == second then 0.0
      else
        val firstOffset = first.ordinal * 3
        val secondOffset = second.ordinal * 3
        val dot =
          (points.packedValue(firstOffset) / firstRadius) * (points.packedValue(secondOffset) / secondRadius) +
            (points.packedValue(firstOffset + 1) / firstRadius) * (points.packedValue(secondOffset + 1) / secondRadius) +
            (points.packedValue(firstOffset + 2) / firstRadius) * (points.packedValue(secondOffset + 2) / secondRadius)
        math.acos(math.max(-1.0, math.min(1.0, dot)))

  /** Strict legacy ratio: min radius * tolerance > max radius. */
  def isSphere[S, F <: Frame[D3]](
      points: PointField[S, D3, F],
      tolerance: Double
  ): Boolean =
    if points.space.size == 0 || !tolerance.isFinite || tolerance <= 1.0 then false
    else
      var minimum = Double.PositiveInfinity
      var maximum = 0.0
      var i = 0
      while i < points.space.size do
        val r = norm(points, i)
        minimum = math.min(minimum, r)
        maximum = math.max(maximum, r)
        i += 1
      minimum > 0.0 && maximum.isFinite && minimum * tolerance > maximum

  def withRadius[S, F <: Frame[D3]](
      points: PointField[S, D3, F],
      radius: Double,
      tolerance: Double
  ): Either[SphereGeometryError, PointField[S, D3, F]] =
    if !radius.isFinite || radius <= 0.0 then
      Left(SphereGeometryError.InvalidRadius(radius))
    else if !tolerance.isFinite || tolerance <= 1.0 then
      Left(SphereGeometryError.InvalidTolerance(tolerance))
    else if !isSphere(points, tolerance) then Left(SphereGeometryError.NotSpherical)
    else
      val result = new Array[Double](points.space.size * 3)
      var vertex = 0
      var error: Option[SphereGeometryError] = None
      while vertex < points.space.size && error.isEmpty do
        val r = norm(points, vertex)
        var axis = 0
        while axis < 3 && error.isEmpty do
          val offset = vertex * 3 + axis
          val value = (points.packedValue(offset) / r) * radius
          if !value.isFinite then
            error = Some(SphereGeometryError.NonFiniteProjection(vertex, axis))
          else result(offset) = value
          axis += 1
        vertex += 1
      error match
        case Some(value) => Left(value)
        case None        =>
          Right(PointField.fromOwnedFiniteDoubles(points.space, points.frame, result))

  private def norm[S, F <: Frame[D3]](points: PointField[S, D3, F], vertex: Int): Double =
    math.hypot(
      math.hypot(points.packedValue(vertex * 3), points.packedValue(vertex * 3 + 1)),
      points.packedValue(vertex * 3 + 2)
    )

  private def angularRadius[S, F <: Frame[D3]](
      points: PointField[S, D3, F],
      vertex: Int
  ): Either[SphereGeometryError, Double] =
    val value = norm(points, vertex)
    if value == 0.0 then Left(SphereGeometryError.ZeroRadius(vertex))
    else if !value.isFinite then Left(SphereGeometryError.NonFiniteRadius(vertex, value))
    else Right(value)
