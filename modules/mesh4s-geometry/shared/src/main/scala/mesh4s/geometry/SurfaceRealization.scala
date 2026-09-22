package mesh4s.geometry

import locus4s.Index
import locus4s.data.VectorField
import mesh4s.TopologyAlignment
import mesh4s.Triangle
import mesh4s.TriangleTopology
import spatial4s.D2
import spatial4s.D3
import spatial4s.Dim
import spatial4s.Dimension
import spatial4s.Frame
import spatial4s.Point
import spatial4s.Vec

enum SurfaceRealizationError derives CanEqual:
  case WrongVertexDomain
  case Coordinates(error: PointFieldError)

  def message: String =
    this match
      case WrongVertexDomain =>
        "point field belongs to another topology owner"
      case Coordinates(error) =>
        error.message

final case class AxisAlignedBoundingBox[
    D <: Dim,
    F <: Frame[D]
](
    frame: F,
    minimum: Vector[Double],
    maximum: Vector[Double]
)

final case class NondegeneracyTolerance private (
    absoluteArea: Double,
    relativeArea: Double
) derives CanEqual

object NondegeneracyTolerance:
  def create(
      absoluteArea: Double,
      relativeArea: Double
  ): Either[String, NondegeneracyTolerance] =
    if !absoluteArea.isFinite || absoluteArea < 0.0 then
      Left("absolute area tolerance must be finite and non-negative")
    else if !relativeArea.isFinite || relativeArea < 0.0 then
      Left("relative area tolerance must be finite and non-negative")
    else Right(NondegeneracyTolerance(absoluteArea, relativeArea))

  val scaleAware: NondegeneracyTolerance =
    NondegeneracyTolerance(0.0, 1e-12)

final case class DegenerateFace(
    face: Int,
    area: Double,
    requiredArea: Double
) derives CanEqual

final case class NondegeneracyReport(
    degenerateFaces: Vector[DegenerateFace],
    metricIssues: Vector[MetricIssue]
) derives CanEqual:
  def message: String =
    val faces =
      degenerateFaces.map: issue =>
        s"face ${issue.face} has area ${issue.area}, requiring " +
          issue.requiredArea
    (faces ++ metricIssues.map(_.message)).mkString("\n")

/** Finite coordinates for one exact topology in one typed frame. */
final class SurfaceRealization[
    T <: TriangleTopology,
    D <: Dim,
    F <: Frame[D]
] private[geometry] (
    val topology: T,
    val positions: PointField[topology.Vertex, D, F]
)(using val dimension: Dimension[D]):
  val frame: F =
    positions.frame

  def position(
      vertex: Index[topology.Vertex]
  ): Point[positions.frame.type, D] =
    positions.point(vertex)

  def edgeVector(
      edge: Index[topology.Edge]
  ): Vec[positions.frame.type, D] =
    val endpoints = topology.endpointsOf(edge)
    val values =
      Vector.tabulate(dimension.rank): axis =>
        coordinate(endpoints.second, axis) - coordinate(endpoints.first, axis)
    Vec
      .fromVector(positions.frame, values)
      .fold(
        error =>
          throw new IllegalStateException(
            s"validated realization produced ${error.message}"
          ),
        identity
      )

  def squaredEdgeLength(edge: Index[topology.Edge]): Double =
    val endpoints = topology.endpointsOf(edge)
    var axis = 0
    var result = 0.0
    while axis < dimension.rank do
      val difference =
        coordinate(endpoints.second, axis) - coordinate(endpoints.first, axis)
      result += difference * difference
      axis += 1
    result

  def edgeLength(edge: Index[topology.Edge]): Double =
    math.sqrt(squaredEdgeLength(edge))

  def faceCentroid(
      face: Index[topology.Face]
  ): Point[positions.frame.type, D] =
    val vertices = topology.verticesOf(face)
    val values =
      Vector.tabulate(dimension.rank): axis =>
        (
          coordinate(vertices.first, axis) +
            coordinate(vertices.second, axis) +
            coordinate(vertices.third, axis)
        ) / 3.0
    Point
      .fromVector(positions.frame, values)
      .fold(
        error =>
          throw new IllegalStateException(
            s"validated realization produced ${error.message}"
          ),
        identity
      )

  def boundingBox: Option[AxisAlignedBoundingBox[D, F]] =
    if topology.vertices.size == 0 then None
    else
      val minimum = Array.fill(dimension.rank)(Double.PositiveInfinity)
      val maximum = Array.fill(dimension.rank)(Double.NegativeInfinity)
      topology.vertices.foreachIndex: vertex =>
        var axis = 0
        while axis < dimension.rank do
          val value = coordinate(vertex, axis)
          minimum(axis) = math.min(minimum(axis), value)
          maximum(axis) = math.max(maximum(axis), value)
          axis += 1
      Some(AxisAlignedBoundingBox(frame, minimum.toVector, maximum.toVector))

  def unsignedFaceArea(face: Index[topology.Face]): Double =
    val vertices = topology.verticesOf(face)
    val first = difference(vertices.second, vertices.first)
    val second = difference(vertices.third, vertices.first)
    val firstSquared = dot(first, first)
    val secondSquared = dot(second, second)
    val product = dot(first, second)
    0.5 * math.sqrt(
      math.max(0.0, firstSquared * secondSquared - product * product)
    )

  def totalArea: Double =
    var result = 0.0
    topology.faces.foreachIndex(face => result += unsignedFaceArea(face))
    result

  def requireNondegenerate(
      tolerance: NondegeneracyTolerance
  ): Either[
    NondegeneracyReport,
    NondegenerateRealization[T, D, F, this.type]
  ] =
    val lengths =
      VectorField.tabulate(topology.edges)(edge => edgeLength(edge))
    val metricResult =
      PiecewiseEuclideanMetric.fromField(topology, frame.unit, lengths)
    val faceAreas =
      VectorField.tabulate(topology.faces)(face => unsignedFaceArea(face))
    val degenerate = Vector.newBuilder[DegenerateFace]
    topology.faces.foreachIndex: face =>
      val vertices = topology.verticesOf(face)
      val firstSquared = squaredDistance(vertices.first, vertices.second)
      val secondSquared = squaredDistance(vertices.second, vertices.third)
      val thirdSquared = squaredDistance(vertices.third, vertices.first)
      val scale = math.max(firstSquared, math.max(secondSquared, thirdSquared))
      val required =
        math.max(tolerance.absoluteArea, tolerance.relativeArea * scale)
      val area = faceAreas(face)
      if !area.isFinite || area <= required then
        degenerate += DegenerateFace(face.ordinal, area, required)
    val faceIssues = degenerate.result()
    metricResult match
      case Left(audit) =>
        Left(NondegeneracyReport(faceIssues, audit.issues))
      case Right(metric) if faceIssues.nonEmpty =>
        Left(NondegeneracyReport(faceIssues, Vector.empty))
      case Right(metric) =>
        Right(new NondegenerateRealization(this, faceAreas, metric))

  def rebind[R <: TriangleTopology](
      alignment: TopologyAlignment[topology.type, R]
  ): SurfaceRealization[R, D, F] =
    new SurfaceRealization(
      alignment.right,
      positions.rebind(alignment.vertices)
    )

  private[geometry] def coordinate(
      vertex: Index[topology.Vertex],
      axis: Int
  ): Double =
    positions.coordinate(vertex, axis).getOrElse {
      throw new IndexOutOfBoundsException(
        s"axis $axis is outside dimension ${dimension.rank}"
      )
    }

  private[geometry] def difference(
      to: Index[topology.Vertex],
      from: Index[topology.Vertex]
  ): Array[Double] =
    Array.tabulate(dimension.rank)(axis => coordinate(to, axis) - coordinate(from, axis))

  private def squaredDistance(
      first: Index[topology.Vertex],
      second: Index[topology.Vertex]
  ): Double =
    val vector = difference(first, second)
    dot(vector, vector)

  private def dot(first: Array[Double], second: Array[Double]): Double =
    var axis = 0
    var result = 0.0
    while axis < first.length do
      result += first(axis) * second(axis)
      axis += 1
    result

object SurfaceRealization:
  def fromPointField[
      T <: TriangleTopology,
      D <: Dim,
      F <: Frame[D]
  ](
      topology: T,
      positions: PointField[topology.Vertex, D, F]
  )(using
      Dimension[D]
  ): Either[
    SurfaceRealizationError,
    SurfaceRealization[topology.type, D, F]
  ] =
    if !topology.vertices.sameRuntimeOwnerAs(positions.space) then
      Left(SurfaceRealizationError.WrongVertexDomain)
    else
      Right(
        new SurfaceRealization[topology.type, D, F](
          topology,
          positions
        )
      )

  def fromInterleavedDoubles[T <: TriangleTopology, D <: Dim](
      topology: T,
      frame: Frame[D],
      coordinates: Array[Double]
  )(using
      Dimension[D]
  ): Either[
    SurfaceRealizationError,
    SurfaceRealization[topology.type, D, Frame[D]]
  ] =
    PointField
      .fromInterleavedDoubles(topology.vertices, frame, coordinates)
      .left
      .map(SurfaceRealizationError.Coordinates.apply)
      .flatMap(fromPointField(topology, _))

final class NondegenerateRealization[
    T <: TriangleTopology,
    D <: Dim,
    F <: Frame[D],
    R <: SurfaceRealization[T, D, F]
] private[geometry] (
    val realization: R,
    val faceAreas: VectorField[realization.topology.Face, Double],
    val metric: PiecewiseEuclideanMetric[T]
):
  val topology: T =
    realization.topology

  def totalArea: Double =
    faceAreas.toVector.sum

final case class BarycentricCoordinates(
    first: Double,
    second: Double,
    third: Double,
    offPlaneResidual: Double
):
  def interpolate(values: Triangle[Double]): Double =
    first * values.first + second * values.second + third * values.third

object SurfaceGeometry:
  def barycentric[
      T <: TriangleTopology,
      D <: Dim,
      F <: Frame[D],
      R <: SurfaceRealization[T, D, F]
  ](
      surface: NondegenerateRealization[T, D, F, R],
      face: Index[surface.realization.topology.Face],
      query: Point[surface.realization.positions.frame.type, D]
  ): BarycentricCoordinates =
    val vertices = surface.realization.topology.verticesOf(face)
    val origin = surface.realization.position(vertices.first)
    val first =
      surface.realization.difference(vertices.second, vertices.first)
    val second =
      surface.realization.difference(vertices.third, vertices.first)
    val queryVector =
      Array.tabulate(surface.realization.dimension.rank): axis =>
        query.coordinate(axis).get - origin.coordinate(axis).get
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
      Array.tabulate(surface.realization.dimension.rank): axis =>
        queryVector(axis) -
          secondWeight * first(axis) -
          thirdWeight * second(axis)
    BarycentricCoordinates(
      firstWeight,
      secondWeight,
      thirdWeight,
      math.sqrt(dot(residual, residual))
    )

  private def dot(first: Array[Double], second: Array[Double]): Double =
    var index = 0
    var result = 0.0
    while index < first.length do
      result += first(index) * second(index)
      index += 1
    result

object SurfaceGeometry2:
  def signedFaceArea[T <: TriangleTopology, F <: Frame[D2]](
      realization: SurfaceRealization[T, D2, F],
      face: Index[realization.topology.Face]
  ): Double =
    val vertices = realization.topology.verticesOf(face)
    val firstX =
      realization.coordinate(vertices.second, 0) -
        realization.coordinate(vertices.first, 0)
    val firstY =
      realization.coordinate(vertices.second, 1) -
        realization.coordinate(vertices.first, 1)
    val secondX =
      realization.coordinate(vertices.third, 0) -
        realization.coordinate(vertices.first, 0)
    val secondY =
      realization.coordinate(vertices.third, 1) -
        realization.coordinate(vertices.first, 1)
    0.5 * (firstX * secondY - firstY * secondX)

object SurfaceGeometry3:
  def vectorArea[T <: TriangleTopology, F <: Frame[D3]](
      realization: SurfaceRealization[T, D3, F],
      face: Index[realization.topology.Face]
  ): Vec[realization.positions.frame.type, D3] =
    val vertices = realization.topology.verticesOf(face)
    val first = realization.difference(vertices.second, vertices.first)
    val second = realization.difference(vertices.third, vertices.first)
    val values =
      Vector(
        0.5 * (first(1) * second(2) - first(2) * second(1)),
        0.5 * (first(2) * second(0) - first(0) * second(2)),
        0.5 * (first(0) * second(1) - first(1) * second(0))
      )
    Vec
      .fromVector(realization.positions.frame, values)
      .fold(error => throw new IllegalStateException(error.message), identity)

  def unitFaceNormal[
      T <: TriangleTopology,
      F <: Frame[D3],
      R <: SurfaceRealization[T, D3, F]
  ](
      surface: NondegenerateRealization[T, D3, F, R],
      face: Index[surface.realization.topology.Face]
  ): Vec[surface.realization.positions.frame.type, D3] =
    val vector = vectorArea(surface.realization, face)
    val coordinates = vector.coordinates
    val norm = math.sqrt(coordinates.map(value => value * value).sum)
    Vec
      .fromVector(
        surface.realization.positions.frame,
        coordinates.map(_ / norm)
      )
      .fold(error => throw new IllegalStateException(error.message), identity)
