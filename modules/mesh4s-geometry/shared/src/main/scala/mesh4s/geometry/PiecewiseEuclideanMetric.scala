package mesh4s.geometry

import locus4s.Index
import locus4s.data.Field
import locus4s.data.FieldConstructionError
import locus4s.data.VectorField
import mesh4s.TriangleTopology
import spatial4s.CoordinateUnit

enum MetricIssue derives CanEqual:
  case NonFiniteEdge(edge: Int, value: Double)
  case NonPositiveEdge(edge: Int, value: Double)
  case TriangleInequality(
      face: Int,
      first: Double,
      second: Double,
      third: Double
  )
  case WrongEdgeDomain
  case WrongEdgeCount(expected: Int, actual: Int)

  def message: String =
    this match
      case NonFiniteEdge(edge, value) =>
        s"edge $edge has non-finite length $value"
      case NonPositiveEdge(edge, value) =>
        s"edge $edge has non-positive length $value"
      case TriangleInequality(face, first, second, third) =>
        s"face $face violates a strict triangle inequality: " +
          s"($first, $second, $third)"
      case WrongEdgeDomain =>
        "edge-length field belongs to another topology owner"
      case WrongEdgeCount(expected, actual) =>
        s"metric requires $expected edge lengths, found $actual"

final case class MetricAudit(issues: Vector[MetricIssue]) derives CanEqual:
  def message: String =
    issues.map(_.message).mkString("\n")

final case class MetricConditioningPolicy private (
    relativeSlack: Double
) derives CanEqual

object MetricConditioningPolicy:
  def create(relativeSlack: Double): Either[String, MetricConditioningPolicy] =
    if relativeSlack.isFinite && relativeSlack >= 0.0 then
      Right(MetricConditioningPolicy(relativeSlack))
    else Left(s"relative triangle slack must be finite and non-negative")

final case class IllConditionedFace(
    face: Int,
    slack: Double,
    requiredSlack: Double
) derives CanEqual

/** Intrinsic edge lengths over one exact triangle topology. */
final class PiecewiseEuclideanMetric[T <: TriangleTopology] private (
    val topology: T,
    val unit: CoordinateUnit,
    val edgeLengths: Field[topology.Edge, Double]
):
  def length(edge: Index[topology.Edge]): Double =
    edgeLengths(edge)

  def faceArea(face: Index[topology.Face]): Double =
    val lengths = faceLengths(face)
    val first = lengths(0)
    val second = lengths(1)
    val third = lengths(2)
    val product =
      (first + second + third) *
        (-first + second + third) *
        (first - second + third) *
        (first + second - third)
    0.25 * math.sqrt(math.max(0.0, product))

  def conditioningFailures(
      policy: MetricConditioningPolicy
  ): Vector[IllConditionedFace] =
    val failures = Vector.newBuilder[IllConditionedFace]
    topology.faces.foreachIndex: face =>
      val lengths = faceLengths(face)
      val first = lengths(0)
      val second = lengths(1)
      val third = lengths(2)
      val slack =
        math.min(
          first + second - third,
          math.min(second + third - first, third + first - second)
        )
      val required =
        policy.relativeSlack * math.max(first, math.max(second, third))
      if slack <= required then
        failures += IllConditionedFace(face.ordinal, slack, required)
    failures.result()

  private def faceLengths(face: Index[topology.Face]): Array[Double] =
    val output = Array.ofDim[Double](3)
    var local = 0
    topology.foreachFaceHalfedge(face): halfedge =>
      output(local) = edgeLengths(topology.edgeOf(halfedge))
      local += 1
    output

object PiecewiseEuclideanMetric:
  def fromField[T <: TriangleTopology](
      topology: T,
      unit: CoordinateUnit,
      edgeLengths: Field[topology.Edge, Double]
  ): Either[MetricAudit, PiecewiseEuclideanMetric[T]] =
    if !topology.edges.sameRuntimeOwnerAs(edgeLengths.space) then
      Left(MetricAudit(Vector(MetricIssue.WrongEdgeDomain)))
    else
      val issues = Vector.newBuilder[MetricIssue]
      topology.edges.foreachIndex: edge =>
        val value = edgeLengths(edge)
        if !value.isFinite then issues += MetricIssue.NonFiniteEdge(edge.ordinal, value)
        else if value <= 0.0 then issues += MetricIssue.NonPositiveEdge(edge.ordinal, value)
      topology.faces.foreachIndex: face =>
        val values = Array.ofDim[Double](3)
        var local = 0
        topology.foreachFaceHalfedge(face): halfedge =>
          values(local) = edgeLengths(topology.edgeOf(halfedge))
          local += 1
        val first = values(0)
        val second = values(1)
        val third = values(2)
        if first.isFinite && second.isFinite && third.isFinite &&
          first > 0.0 && second > 0.0 && third > 0.0 &&
          (!(first + second > third) ||
            !(second + third > first) ||
            !(third + first > second))
        then
          issues +=
            MetricIssue.TriangleInequality(
              face.ordinal,
              first,
              second,
              third
            )
      val result = issues.result()
      if result.nonEmpty then Left(MetricAudit(result))
      else Right(new PiecewiseEuclideanMetric(topology, unit, edgeLengths))

  def fromValues[T <: TriangleTopology](
      topology: T,
      unit: CoordinateUnit,
      values: IterableOnce[Double]
  ): Either[MetricAudit, PiecewiseEuclideanMetric[T]] =
    VectorField
      .fromValues(topology.edges, values)
      .left
      .map:
        case FieldConstructionError.WrongValueCount(expected, actual) =>
          MetricAudit(Vector(MetricIssue.WrongEdgeCount(expected, actual)))
      .flatMap(fromField(topology, unit, _))
