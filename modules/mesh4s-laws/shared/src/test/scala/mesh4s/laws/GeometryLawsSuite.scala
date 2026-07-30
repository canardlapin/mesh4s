package mesh4s.laws

import mesh4s.Triangle
import mesh4s.TriangleTopology
import mesh4s.geometry.NondegeneracyTolerance
import mesh4s.geometry.SurfaceGeometry
import mesh4s.geometry.SurfaceGeometry2
import mesh4s.geometry.SurfaceGeometry3
import mesh4s.geometry.SurfaceRealization
import mesh4s.geometry.SurfaceRealizationError
import mesh4s.reference.ReferenceGeometry
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import spatial4s.D2
import spatial4s.D3
import spatial4s.Frame
import spatial4s.Point

class GeometryLawsSuite extends munit.ScalaCheckSuite:
  property("D2 packed geometry agrees with an independent oracle under rigid motion"):
    forAll(rigid2Generator) { (angle, translationX, translationY) =>
      val cosine = math.cos(angle)
      val sine = math.sin(angle)
      val base = Vector(Vector(0.0, 0.0), Vector(3.0, 0.0), Vector(0.0, 4.0))
      val points =
        base.map: point =>
          Vector(
            cosine * point(0) - sine * point(1) + translationX,
            sine * point(0) + cosine * point(1) + translationY
          )
      val surface = realization2(points)
      val certified =
        surface
          .requireNondegenerate(tolerance)
          .fold(report => fail(report.message), identity)
      val face = index(surface.topology.faces, 0)
      val referenceFace = Vector(0, 1, 2)
      assertClose(
        surface.unsignedFaceArea(face),
        ReferenceGeometry.unsignedArea(points, referenceFace),
        areaTolerance
      )
      assertClose(
        SurfaceGeometry2.signedFaceArea(surface, face),
        ReferenceGeometry.signedArea2(points, referenceFace),
        areaTolerance
      )
      val centroid = surface.faceCentroid(face).coordinates
      assertVectorClose(
        centroid,
        ReferenceGeometry.centroid(points, referenceFace),
        coordinateTolerance
      )
      surface.topology.edges.foreachIndex: edge =>
        val endpoints = surface.topology.endpointsOf(edge)
        assertClose(
          surface.edgeLength(edge),
          ReferenceGeometry.edgeLength(
            points,
            endpoints.first.ordinal,
            endpoints.second.ordinal
          ),
          lengthTolerance
        )

      val queryCoordinates =
        Vector.tabulate(2): axis =>
          0.2 * points(0)(axis) +
            0.3 * points(1)(axis) +
            0.5 * points(2)(axis)
      val query =
        spatialRight(
          Point.fromVector(surface.positions.frame, queryCoordinates)
        )
      val actual = SurfaceGeometry.barycentric(certified, face, query)
      val expected =
        ReferenceGeometry.barycentric(points, referenceFace, queryCoordinates)
      assertVectorClose(
        Vector(actual.first, actual.second, actual.third),
        expected.weights,
        barycentricTolerance
      )
      assertClose(
        actual.offPlaneResidual,
        expected.offPlaneResidual,
        coordinateTolerance
      )
      assertVectorClose(
        certified.metric.edgeLengths.toVector.sorted,
        Vector(3.0, 4.0, 5.0),
        lengthTolerance
      )
    }

  property("D3 vector area and residual agree with the independent oracle"):
    forAll(rigid3Generator) { (angle, tx, ty, tz, residual) =>
      val cosine = math.cos(angle)
      val sine = math.sin(angle)
      val base =
        Vector(
          Vector(0.0, 0.0, 0.0),
          Vector(3.0, 0.0, 0.0),
          Vector(0.0, 4.0, 0.0)
        )
      val points =
        base.map: point =>
          Vector(
            cosine * point(0) - sine * point(1) + tx,
            sine * point(0) + cosine * point(1) + ty,
            point(2) + tz
          )
      val surface = realization3(points)
      val certified =
        surface
          .requireNondegenerate(tolerance)
          .fold(report => fail(report.message), identity)
      val face = index(surface.topology.faces, 0)
      assertVectorClose(
        SurfaceGeometry3.vectorArea(surface, face).coordinates,
        ReferenceGeometry.vectorArea3(points, Vector(0, 1, 2)),
        areaTolerance
      )
      val queryCoordinates =
        Vector(
          0.2 * points(0)(0) + 0.3 * points(1)(0) + 0.5 * points(2)(0),
          0.2 * points(0)(1) + 0.3 * points(1)(1) + 0.5 * points(2)(1),
          tz + residual
        )
      val query =
        spatialRight(
          Point.fromVector(surface.positions.frame, queryCoordinates)
        )
      val actual = SurfaceGeometry.barycentric(certified, face, query)
      val expected =
        ReferenceGeometry.barycentric(
          points,
          Vector(0, 1, 2),
          queryCoordinates
        )
      assertVectorClose(
        Vector(actual.first, actual.second, actual.third),
        expected.weights,
        barycentricTolerance
      )
      assertClose(
        actual.offPlaneResidual,
        math.abs(residual),
        coordinateTolerance
      )
      assertClose(
        actual.offPlaneResidual,
        expected.offPlaneResidual,
        coordinateTolerance
      )
    }

  private val lengthTolerance = 1e-11
  private val areaTolerance = 1e-10
  private val coordinateTolerance = 1e-11
  private val barycentricTolerance = 1e-11

  private val tolerance =
    NondegeneracyTolerance
      .create(0.0, 1e-14)
      .fold(error => fail(error), identity)

  private val rigid2Generator =
    for
      angle <- Gen.choose(-math.Pi, math.Pi)
      translationX <- Gen.choose(-1000.0, 1000.0)
      translationY <- Gen.choose(-1000.0, 1000.0)
    yield (angle, translationX, translationY)

  private val rigid3Generator =
    for
      angle <- Gen.choose(-math.Pi, math.Pi)
      translationX <- Gen.choose(-1000.0, 1000.0)
      translationY <- Gen.choose(-1000.0, 1000.0)
      translationZ <- Gen.choose(-1000.0, 1000.0)
      residual <- Gen.choose(-100.0, 100.0)
    yield (angle, translationX, translationY, translationZ, residual)

  private def realization2(
      points: Vector[Vector[Double]]
  ): SurfaceRealization[? <: TriangleTopology, D2, Frame[D2]] =
    val topology = triangleTopology()
    val frame = spatialRight(Frame.named[D2]("law-plane"))
    surfaceRight(
      SurfaceRealization.fromInterleavedDoubles(
        topology,
        frame,
        points.flatten.toArray
      )
    )

  private def realization3(
      points: Vector[Vector[Double]]
  ): SurfaceRealization[? <: TriangleTopology, D3, Frame[D3]] =
    val topology = triangleTopology()
    val frame = spatialRight(Frame.named[D3]("law-world"))
    surfaceRight(
      SurfaceRealization.fromInterleavedDoubles(
        topology,
        frame,
        points.flatten.toArray
      )
    )

  private def triangleTopology(): TriangleTopology =
    TriangleTopology
      .fromOrdinalFaces(3, Vector(Triangle(0, 1, 2)))
      .fold(audit => fail(audit.message), identity)

  private def surfaceRight[
      T <: TriangleTopology,
      D <: spatial4s.Dim,
      F <: Frame[D]
  ](
      value: Either[SurfaceRealizationError, SurfaceRealization[T, D, F]]
  ): SurfaceRealization[T, D, F] =
    value.fold(error => fail(error.message), identity)

  private def spatialRight[A](
      value: Either[spatial4s.SpatialError, A]
  ): A =
    value.fold(error => fail(error.message), identity)

  private def index[S](
      domain: locus4s.FiniteDomain[S],
      ordinal: Int
  ): locus4s.Index[S] =
    domain.index(ordinal).fold(error => fail(error.message), identity)

  private def assertClose(
      actual: Double,
      expected: Double,
      tolerance: Double
  ): Unit =
    assert(
      math.abs(actual - expected) <= tolerance,
      s"expected $expected +/- $tolerance, found $actual"
    )

  private def assertVectorClose(
      actual: Vector[Double],
      expected: Vector[Double],
      tolerance: Double
  ): Unit =
    assertEquals(actual.length, expected.length)
    actual.zip(expected).foreach((left, right) => assertClose(left, right, tolerance))
