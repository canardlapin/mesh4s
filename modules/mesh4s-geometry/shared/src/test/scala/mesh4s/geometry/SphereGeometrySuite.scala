package mesh4s.geometry

import mesh4s.Triangle
import mesh4s.TriangleTopology
import spatial4s.D3
import spatial4s.Frame

class SphereGeometrySuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)

  test("projection retains the exact vertex domain and frame and leaves input immutable"):
    val owner = ok(TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))))
    val frame = ok(Frame.named[D3]("sphere"))
    val coordinates = Array(3.0, 4.0, 0.0, 0.0, 0.0, 5.0, -5.0, 0.0, 0.0)
    val points = ok(PointField.fromInterleavedDoubles(owner.vertices, frame, coordinates))
    val projected = ok(SphereGeometry.withRadius(points, 100, 1.001))
    assert(projected.space.sameRuntimeOwnerAs(points.space))
    assert(projected.frame eq points.frame)
    assertEquals(points.interleavedDoubles.toVector, coordinates.toVector)
    projected.space.foreachIndex(i =>
      assertEqualsDouble(SphereGeometry.radius(projected, i), 100, 1e-12)
    )
    assertEqualsDouble(projected.packedValue(0), 60, 1e-12)
    assertEqualsDouble(projected.packedValue(1), 80, 1e-12)

  test(
    "explicit sphere admission handles invalid policies, zero radii, and extreme finite magnitudes"
  ):
    val owner = ok(TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))))
    val frame = ok(Frame.named[D3]("sphere"))
    Vector(1e-200, 1.0, 1e200).foreach: radius =>
      val points = ok(
        PointField.fromInterleavedDoubles(
          owner.vertices,
          frame,
          Array(radius, 0, 0, 0, radius, 0, 0, 0, radius)
        )
      )
      assert(SphereGeometry.isSphere(points, 1.001))
      val normalized = ok(SphereGeometry.withRadius(points, 100, 1.001))
      assertEqualsDouble(normalized.packedValue(0), 100, 1e-12)
      assert(SphereGeometry.withRadius(points, 0, 1.001).isLeft)
      assert(SphereGeometry.withRadius(points, 1, 1).isLeft)
      assert(!SphereGeometry.isSphere(points, Double.NaN))
    val zero =
      ok(PointField.fromInterleavedDoubles(owner.vertices, frame, Array.fill(9)(0.0)))
    assert(!SphereGeometry.isSphere(zero, 1.001))
    assert(SphereGeometry.withRadius(zero, 100, 1.001).isLeft)
