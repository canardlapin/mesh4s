package mesh4s.geometry

import mesh4s.Triangle
import mesh4s.TriangleTopology
import spatial4s.CoordinateUnit
import spatial4s.D2
import spatial4s.D3
import spatial4s.Frame
import spatial4s.Point

class SurfaceGeometrySuite extends munit.FunSuite:
  test("packed double and float point fields copy input and export in order"):
    val topology = triangleTopology()
    val frame = right(Frame.named[D2]("plane"))
    val doubles = Array(0.0, 0.0, 3.0, 0.0, 0.0, 4.0)
    val doubleField =
      pointRight(
        PointField.fromInterleavedDoubles(topology.vertices, frame, doubles)
      )
    doubles(0) = 99.0
    assertEquals(doubleField.precision, PointStoragePrecision.Float64)
    assertEquals(doubleField.interleavedDoubles.toVector, coordinates2)

    val floats = coordinates2.map(_.toFloat).toArray
    val floatField =
      pointRight(
        PointField.fromInterleavedFloats(topology.vertices, frame, floats)
      )
    floats(1) = 99.0f
    assertEquals(floatField.precision, PointStoragePrecision.Float32)
    assertEquals(floatField.interleavedDoubles.toVector, coordinates2)

    var checksum = 0.0
    doubleField.foreachD2((_, x, y) => checksum += x + y)
    assertEquals(checksum, 7.0)

  test("point fields reject wrong counts and non-finite coordinates"):
    val topology = triangleTopology()
    val frame = right(Frame.named[D2]("plane"))
    assert(
      PointField
        .fromInterleavedDoubles(topology.vertices, frame, Array(0.0))
        .isLeft
    )
    val values = coordinates2.toArray
    values(3) = Double.NaN
    assert(
      PointField
        .fromInterleavedDoubles(topology.vertices, frame, values)
        .isLeft
    )

  test("D2 realization exposes unambiguous local geometry"):
    val surface = realization2(coordinates2)
    val face = index(surface.topology.faces, 0)
    val centroid = surface.faceCentroid(face)
    assertClose(centroid.coordinate(0).get, 1.0)
    assertClose(centroid.coordinate(1).get, 4.0 / 3.0)
    assertClose(SurfaceGeometry2.signedFaceArea(surface, face), 6.0)
    assertClose(surface.unsignedFaceArea(face), 6.0)
    assertClose(surface.totalArea, 6.0)
    val bounds = surface.boundingBox.get
    assertEquals(bounds.minimum, Vector(0.0, 0.0))
    assertEquals(bounds.maximum, Vector(3.0, 4.0))

  test("nondegenerate certification retains metric and face-area evidence"):
    val surface = realization2(coordinates2)
    val certified =
      surface
        .requireNondegenerate(tolerance)
        .fold(report => fail(report.message), identity)
    assertClose(certified.totalArea, 6.0)
    assertEquals(
      certified.metric.edgeLengths.toVector.sorted,
      Vector(3.0, 4.0, 5.0)
    )
    val metricFace = index(certified.metric.topology.faces, 0)
    assertClose(certified.metric.faceArea(metricFace), 6.0)

  test("exact metric triangle inequalities reject collapsed stored doubles"):
    val topology = triangleTopology()
    val valid =
      PiecewiseEuclideanMetric.fromValues(
        topology,
        CoordinateUnit.Millimeter,
        Vector(3.0, 5.0, 4.0)
      )
    assert(valid.isRight)
    val collapsed =
      PiecewiseEuclideanMetric.fromValues(
        topology,
        CoordinateUnit.Millimeter,
        Vector(1.0, 1.0, 2.0)
      )
    assert(collapsed.isLeft)

  test("conditioning policy strengthens but never relaxes metric validity"):
    val topology = triangleTopology()
    val metric =
      PiecewiseEuclideanMetric
        .fromValues(
          topology,
          CoordinateUnit.Millimeter,
          Vector(3.0, 5.0, 4.0)
        )
        .fold(audit => fail(audit.message), identity)
    val policy =
      MetricConditioningPolicy
        .create(0.5)
        .fold(error => fail(error), identity)
    assertEquals(metric.conditioningFailures(policy).map(_.face), Vector(0))

  test("collinear realization reports area and numerical metric witnesses"):
    val surface = realization2(Vector(0.0, 0.0, 1.0, 0.0, 2.0, 0.0))
    val report =
      surface.requireNondegenerate(tolerance) match
        case Left(value) => value
        case Right(_)    => fail("collinear triangle unexpectedly certified")
    assertEquals(report.degenerateFaces.map(_.face), Vector(0))
    assert(
      report.metricIssues.exists(_.isInstanceOf[MetricIssue.TriangleInequality])
    )

  test("D2 barycentric coordinates interpolate in-plane values"):
    val surface = realization2(coordinates2)
    val certified =
      surface
        .requireNondegenerate(tolerance)
        .fold(report => fail(report.message), identity)
    val face = index(surface.topology.faces, 0)
    val query =
      right(Point.in(surface.positions.frame)(0.75, 1.0))
    val weights = SurfaceGeometry.barycentric(certified, face, query)
    assertClose(weights.first, 0.5)
    assertClose(weights.second, 0.25)
    assertClose(weights.third, 0.25)
    assertClose(weights.offPlaneResidual, 0.0)
    assertClose(weights.interpolate(Triangle(2.0, 6.0, 10.0)), 5.0)

  test("D3 vector area, normal, and off-plane residual are explicit"):
    val topology = triangleTopology()
    val frame = right(Frame.named[D3]("world"))
    val surface =
      surfaceRight(
        SurfaceRealization.fromInterleavedDoubles(
          topology,
          frame,
          Array(0.0, 0.0, 0.0, 3.0, 0.0, 0.0, 0.0, 4.0, 0.0)
        )
      )
    val certified =
      surface
        .requireNondegenerate(tolerance)
        .fold(report => fail(report.message), identity)
    val face = index(surface.topology.faces, 0)
    assertEquals(
      SurfaceGeometry3.vectorArea(surface, face).coordinates,
      Vector(0.0, 0.0, 6.0)
    )
    assertEquals(
      SurfaceGeometry3.unitFaceNormal(certified, face).coordinates,
      Vector(0.0, 0.0, 1.0)
    )
    val query =
      right(Point.in(surface.positions.frame)(0.75, 1.0, 2.0))
    val weights = SurfaceGeometry.barycentric(certified, face, query)
    assertClose(weights.first, 0.5)
    assertClose(weights.second, 0.25)
    assertClose(weights.third, 0.25)
    assertClose(weights.offPlaneResidual, 2.0)

  private val coordinates2 =
    Vector(0.0, 0.0, 3.0, 0.0, 0.0, 4.0)

  private val tolerance =
    NondegeneracyTolerance
      .create(0.0, 1e-14)
      .fold(error => fail(error), identity)

  private def triangleTopology(): TriangleTopology =
    TriangleTopology
      .fromOrdinalFaces(3, Vector(Triangle(0, 1, 2)))
      .fold(audit => fail(audit.message), identity)

  private def realization2(
      coordinates: Vector[Double]
  ): SurfaceRealization[? <: TriangleTopology, D2, Frame[D2]] =
    val topology = triangleTopology()
    val frame = right(Frame.named[D2]("plane"))
    surfaceRight(
      SurfaceRealization.fromInterleavedDoubles(
        topology,
        frame,
        coordinates.toArray
      )
    )

  private def index[S](
      domain: locus4s.FiniteDomain[S],
      ordinal: Int
  ): locus4s.Index[S] =
    domain.index(ordinal).fold(error => fail(error.message), identity)

  private def right[A](value: Either[spatial4s.SpatialError, A]): A =
    value.fold(error => fail(error.message), identity)

  private def pointRight[S, D <: spatial4s.Dim, F <: Frame[D]](
      value: Either[PointFieldError, PointField[S, D, F]]
  ): PointField[S, D, F] =
    value.fold(error => fail(error.message), identity)

  private def surfaceRight[
      T <: TriangleTopology,
      D <: spatial4s.Dim,
      F <: Frame[D]
  ](
      value: Either[SurfaceRealizationError, SurfaceRealization[T, D, F]]
  ): SurfaceRealization[T, D, F] =
    value.fold(error => fail(error.message), identity)

  private def assertClose(actual: Double, expected: Double): Unit =
    assert(
      math.abs(actual - expected) <= 1e-12,
      s"expected $expected, found $actual"
    )
