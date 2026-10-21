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

  test("angular distance uses endpoint normalization without spherical admission"):
    val owner = ok(TriangleTopology.fromOrdinalFaces(4, Vector(Triangle(0, 1, 2), Triangle(1, 3, 2))))
    val frame = ok(Frame.named[D3]("angular"))
    val points = ok(
      PointField.fromInterleavedDoubles(
        owner.vertices,
        frame,
        Array(2.0, 0.0, 0.0, 0.0, 3.0, 0.0, -5.0, 0.0, 0.0, 1.0, 2.0, 3.0)
      )
    )
    val first = points.space.index(0).toOption.get
    val second = points.space.index(1).toOption.get
    val opposite = points.space.index(2).toOption.get
    val nonAxis = points.space.index(3).toOption.get
    assertEqualsDouble(ok(SphereGeometry.angularDistance(points, first, second)), math.Pi / 2.0, 1e-15)
    assertEqualsDouble(ok(SphereGeometry.angularDistance(points, first, opposite)), math.Pi, 1e-15)
    assertEqualsDouble(ok(SphereGeometry.angularDistance(points, nonAxis, nonAxis)), 0.0, 0.0)

    val extreme = ok(
      PointField.fromInterleavedDoubles(
        owner.vertices,
        frame,
        Array(1e-200, 0.0, 0.0, 0.0, 1e200, 0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 1.0)
      )
    )
    assertEqualsDouble(
      ok(SphereGeometry.angularDistance(extreme, extreme.space.index(0).toOption.get, extreme.space.index(1).toOption.get)),
      math.Pi / 2.0,
      1e-15
    )

  test("angular distance rejects a zero or non-finite endpoint norm before same-index zero"):
    val owner = ok(TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))))
    val frame = ok(Frame.named[D3]("angular-errors"))
    val zero = ok(
      PointField.fromInterleavedDoubles(
        owner.vertices,
        frame,
        Array(0.0, 0.0, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0)
      )
    )
    val zeroVertex = zero.space.index(0).toOption.get
    assertEquals(
      SphereGeometry.angularDistance(zero, zeroVertex, zeroVertex),
      Left(SphereGeometryError.ZeroRadius(0))
    )

    val overflowing = ok(
      PointField.fromInterleavedDoubles(
        owner.vertices,
        frame,
        Array(Double.MaxValue, Double.MaxValue, 0.0, 1.0, 0.0, 0.0, 0.0, 1.0, 0.0)
      )
    )
    val overflowingVertex = overflowing.space.index(0).toOption.get
    assertEquals(
      SphereGeometry.angularDistance(overflowing, overflowingVertex, overflowing.space.index(1).toOption.get),
      Left(SphereGeometryError.NonFiniteRadius(0, Double.PositiveInfinity))
    )

  test("angular distance retains subnormal endpoint directions"):
    val owner = ok(TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))))
    val frame = ok(Frame.named[D3]("subnormal-angular"))
    val points = ok(
      PointField.fromInterleavedDoubles(
        owner.vertices,
        frame,
        Array(2e-162, 2e-162, 0.0, 2e-162, 0.0, 0.0, 1.0, 0.0, 0.0)
      )
    )
    assertEqualsDouble(
      ok(SphereGeometry.angularDistance(points, points.space.index(0).toOption.get, points.space.index(1).toOption.get)),
      math.Pi / 4.0,
      1e-15
    )

  test("angular distance preserves minimum-subnormal directions and matches unit geometry"):
    val owner = ok(TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))))
    val frame = ok(Frame.named[D3]("minimum-subnormal-angular"))
    val minimum = java.lang.Double.MIN_VALUE
    val subnormal = ok(
      PointField.fromInterleavedDoubles(
        owner.vertices,
        frame,
        Array(minimum, minimum, 0.0, minimum, 0.0, 0.0, 1.0, 0.0, 0.0)
      )
    )
    val unit = ok(
      PointField.fromInterleavedDoubles(
        owner.vertices,
        frame,
        Array(1.0, 1.0, 0.0, 1.0, 0.0, 0.0, 1.0, 0.0, 0.0)
      )
    )
    val subnormalAngle = ok(
      SphereGeometry.angularDistance(subnormal, subnormal.space.index(0).toOption.get, subnormal.space.index(1).toOption.get)
    )
    val unitAngle = ok(
      SphereGeometry.angularDistance(unit, unit.space.index(0).toOption.get, unit.space.index(1).toOption.get)
    )
    assertEqualsDouble(subnormalAngle, math.Pi / 4.0, 1e-15)
    assertEqualsDouble(subnormalAngle, unitAngle, 1e-15)

  test("angular distance requires an index owned by its point field at compile time"):
    val errors = compileErrors("""
      import mesh4s.*
      import mesh4s.geometry.*
      import spatial4s.{D3, Frame}
      val first = TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))).toOption.get
      val second = TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))).toOption.get
      val frame = Frame.named[D3]("angular-owner").toOption.get
      val points = PointField.fromInterleavedDoubles(first.vertices, frame, Array.fill(9)(1.0)).toOption.get
      SphereGeometry.angularDistance(points, first.vertices.index(0).toOption.get, second.vertices.index(0).toOption.get)
    """)
    assert(errors.nonEmpty)
