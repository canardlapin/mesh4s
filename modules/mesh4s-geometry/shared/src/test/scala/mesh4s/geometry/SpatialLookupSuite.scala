package mesh4s.geometry

import mesh4s.Triangle
import mesh4s.TriangleTopology
import spatial4s.D3
import spatial4s.Frame
import spatial4s.Point

class SpatialLookupSuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)

  private def topology(count: Int): TriangleTopology =
    ok(
      TriangleTopology.fromOrdinalFaces(
        count,
        (0 until count / 3).map(i => Triangle(3 * i, 3 * i + 1, 3 * i + 2))
      )
    )

  test(
    "nearest agrees with exhaustive Euclidean search across anisotropy and outside bounds"
  ):
    val owner = topology(1002)
    val frame = ok(Frame.named[D3]("lookup"))
    val coordinates = Array.tabulate(3006): i =>
      val v = i / 3
      i % 3 match
        case 0 => math.sin(v * 1.7)
        case 1 => 10000 * math.cos(v * 0.31)
        case _ => 0.001 * math.sin(v * 0.7)
    val points = ok(PointField.fromInterleavedDoubles(owner.vertices, frame, coordinates))
    val index = new NearestVertexIndex(points)
    (0 until 130).foreach: n =>
      val query = Vector(math.cos(n * 0.7) * 3, math.sin(n * 0.1) * 20000, n * 0.003)
      // Exhaustive independent scan: no spatial partition or bounds.
      val expected = (0 until 1002).minBy: v =>
        (0 until 3).map(k => math.pow(coordinates(3 * v + k) - query(k), 2)).sum
      assertEquals(
        index
          .nearest(ok(Point.in(index.frame)(query(0), query(1), query(2))))
          .map(_.ordinal),
        Some(expected)
      )

  test(
    "nearest has deterministic ordinal ties, Float32 support, empty domains, and large finite distances"
  ):
    val owner = topology(3)
    val frame = ok(Frame.named[D3]("points"))
    val points = ok(
      PointField.fromInterleavedFloats(
        owner.vertices,
        frame,
        Array(-1f, 0f, 0f, 1f, 0f, 0f, 1f, 0f, 0f)
      )
    )
    val index = new NearestVertexIndex(points)
    assertEquals(index.nearest(ok(Point.in(index.frame)(0, 0, 0))).map(_.ordinal), Some(0))
    val huge = ok(
      PointField.fromInterleavedDoubles(
        owner.vertices,
        frame,
        Array(1e200, 0, 0, 2e200, 0, 0, 3e200, 0, 0)
      )
    )
    val hugeIndex = new NearestVertexIndex(huge)
    assertEquals(
      hugeIndex.nearest(ok(Point.in(hugeIndex.frame)(0, 0, 0))).map(_.ordinal),
      Some(0)
    )
    val overflow = ok(
      PointField.fromInterleavedDoubles(
        owner.vertices,
        frame,
        Array(1.7e308, 0, 0, 1.0e308, 0, 0, 1.2e308, 0, 0)
      )
    )
    val overflowIndex = new NearestVertexIndex(overflow)
    assertEquals(
      overflowIndex.nearest(ok(Point.in(overflowIndex.frame)(-1e308, 0, 0))).map(_.ordinal),
      Some(1)
    )
    val emptyOwner = topology(0)
    val empty = ok(
      PointField.fromInterleavedDoubles(emptyOwner.vertices, frame, Array.emptyDoubleArray)
    )
    val emptyIndex = new NearestVertexIndex(empty)
    assertEquals(emptyIndex.nearest(ok(Point.in(emptyIndex.frame)(0, 0, 0))), None)

  test("lookup queries require the index frame, not merely another D3 frame"):
    val errors = compileErrors("""
      import mesh4s.geometry.*
      import spatial4s.{D3, Frame, Point}
      def wrong[S](index: NearestVertexIndex[S, Frame[D3]]) =
        val other = Frame.named[D3]("other").toOption.get
        index.nearest(Point.in(other)(0, 0, 0).toOption.get)
    """)
    assert(errors.nonEmpty)

  test("radial hierarchy agrees with exhaustive plane and Gram-system oracle"):
    val owner = topology(240)
    val frame = ok(Frame.named[D3]("radial"))
    val coordinates = (0 until 80).flatMap: face =>
      val x = 3.0 * math.sin(face * 1.7)
      val y = 3.0 * math.cos(face * 0.6)
      val z = if face % 2 == 0 then 1.0 + face * 0.03 else -1.0 - face * 0.03
      Vector(
        x - 1,
        y - 1,
        z,
        x + 1,
        y - 0.7,
        z + 0.1 * math.sin(face),
        x,
        y + 1,
        z + 0.2 * math.cos(face)
      )
    val surface =
      ok(SurfaceRealization.fromInterleavedDoubles(owner, frame, coordinates.toArray))
    val index = ok(RadialFaceIndex.create(surface, NondegeneracyTolerance.scaleAware))
    val queries = (0 until 80).map: face =>
      Vector.tabulate(3)(axis =>
        (0 until 3).map(corner => coordinates(9 * face + 3 * corner + axis)).sum / 3
      )
    (queries ++ Vector(Vector(0.0, 0.0, 0.0), Vector(10.0, 10.0, 1.0))).foreach: q =>
      val expected = oracle(coordinates.toArray, q)
      Vector(1e-200, 1.0, 10000.0, 1e200).foreach: scale =>
        val actual =
          index.locate(ok(Point.in(index.frame)(q(0) * scale, q(1) * scale, q(2) * scale)))
        assertEquals(actual.map(_.face.ordinal), expected.map(_._1))
        actual
          .zip(expected)
          .foreach: (hit, wanted) =>
            Vector(hit.weights.first, hit.weights.second, hit.weights.third)
              .zip(wanted._2)
              .foreach: (a, b) =>
                assertEqualsDouble(a, b, 1e-10)

  test(
    "radial admission refuses degeneracy and preserves boundary and disconnected owners"
  ):
    val owner = topology(6)
    val frame = ok(Frame.named[D3]("two components"))
    val coordinates = Array(-1.0, -1.0, 1.0, 1.0, -1.0, 1.0, 0.0, 1.0, 1.0, -1.0, -1.0,
      -1.0, 1.0, -1.0, -1.0, 0.0, 1.0, -1.0)
    val surface = ok(SurfaceRealization.fromInterleavedDoubles(owner, frame, coordinates))
    val index = ok(RadialFaceIndex.create(surface, NondegeneracyTolerance.scaleAware))
    assert(index.surface.topology eq owner)
    assertEquals(owner.componentCount, 2)
    assert(!owner.isClosed)
    val positive = index.locate(ok(Point.in(index.frame)(0, 0, 10000))).get
    assertEquals(positive.face.ordinal, 0)
    assertEqualsDouble(positive.weights.third, 0.5, 1e-12)
    assertEquals(index.locate(ok(Point.in(index.frame)(0, 0, -1))).get.face.ordinal, 1)
    val edge = index.locate(ok(Point.in(index.frame)(0, -1, 1))).get
    assertEqualsDouble(edge.weights.third, 0.0, 1e-12)
    val close = index.locate(ok(Point.in(index.frame)(0, -1.0000005, 1))).get
    assertEqualsDouble(close.weights.third, 0.0, 1e-12)
    assert(index.locate(ok(Point.in(index.frame)(0, -1.00001, 1))).isEmpty)
    val collapsed = coordinates.clone()
    collapsed(6) = 0
    collapsed(7) = -1
    val bad = ok(SurfaceRealization.fromInterleavedDoubles(owner, frame, collapsed))
    assert(RadialFaceIndex.create(bad, NondegeneracyTolerance.scaleAware).isLeft)

  test("radial equal-margin ties select face ordinal and empty surfaces return no hit"):
    val owner = topology(6)
    val frame = ok(Frame.named[D3]("coincident components"))
    val triangle = Array(-1.0, -1.0, 1.0, 1.0, -1.0, 1.0, 0.0, 1.0, 1.0)
    val surface =
      ok(SurfaceRealization.fromInterleavedDoubles(owner, frame, triangle ++ triangle))
    val index = ok(RadialFaceIndex.create(surface, NondegeneracyTolerance.scaleAware))
    assertEquals(index.locate(ok(Point.in(index.frame)(0, 0, 1))).get.face.ordinal, 0)
    val empty = ok(
      SurfaceRealization.fromInterleavedDoubles(topology(0), frame, Array.emptyDoubleArray)
    )
    val emptyIndex = ok(RadialFaceIndex.create(empty, NondegeneracyTolerance.scaleAware))
    assertEquals(emptyIndex.locate(ok(Point.in(emptyIndex.frame)(0, 0, 1))), None)

  // Independent method: intersect the ray with the plane, then solve the
  // two-vector Gram system. Production uses Moller-Trumbore determinants.
  private def oracle(
      points: Array[Double],
      q: Vector[Double]
  ): Option[(Int, Vector[Double])] =
    def dot(a: Vector[Double], b: Vector[Double]): Double = a.zip(b).map(_ * _).sum
    def minus(a: Vector[Double], b: Vector[Double]): Vector[Double] = a.zip(b).map(_ - _)
    val hits = (0 until points.length / 9).flatMap: face =>
      val a = points.slice(face * 9, face * 9 + 3).toVector
      val u = minus(points.slice(face * 9 + 3, face * 9 + 6).toVector, a)
      val v = minus(points.slice(face * 9 + 6, face * 9 + 9).toVector, a)
      val normal = Vector(
        u(1) * v(2) - u(2) * v(1),
        u(2) * v(0) - u(0) * v(2),
        u(0) * v(1) - u(1) * v(0)
      )
      val divisor = dot(normal, q)
      val t = dot(normal, a) / divisor
      if math.abs(divisor) < 1e-15 || t <= 0 || !t.isFinite then None
      else
        val w = minus(q.map(_ * t), a)
        val uu = dot(u, u)
        val uv = dot(u, v)
        val vv = dot(v, v)
        val wu = dot(w, u)
        val wv = dot(w, v)
        val determinant = uu * vv - uv * uv
        val b = (wu * vv - wv * uv) / determinant
        val c = (wv * uu - wu * uv) / determinant
        val weights = Vector(1 - b - c, b, c)
        Option.when(weights.min >= -1e-6)((face, weights))
    hits
      .sortBy((face, weights) => (-weights.min, face))
      .headOption
      .map: (face, weights) =>
        val clamped = weights.map(math.max(0.0, _))
        (face, clamped.map(_ / clamped.sum))
