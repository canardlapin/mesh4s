package mesh4s.geometry

import locus4s.data.VectorField
import mesh4s.{Triangle, TriangleTopology}
import spatial4s.{D2, D3, Frame}

final class CoordinateSmoothingSuite extends munit.FunSuite:
  private def right[E, A](result: Either[E, A]): A =
    result.fold(error => fail(error.toString), identity)

  private def parameters(iterations: Int, step: Double = 0.1): SmoothingParameters =
    right(SmoothingParameters.create(iterations, step))

  private val frame = right(Frame.named[D3]("smoothing"))
  private val faces =
    Vector(Triangle(0, 1, 2), Triangle(0, 2, 3), Triangle(0, 3, 4), Triangle(0, 4, 1))
  private val points =
    Array(0.0, 0.0, 2.0, -1.0, -1.0, 0.0, 1.0, -1.0, 0.0, 1.0, 1.0, 0.0, -1.0, 1.0, 0.0)

  test("hand-computed synchronous updates, fixed boundary, constraint"):
    val topology = right(TriangleTopology.fromOrdinalFaces(5, faces))
    val surface = right(SurfaceRealization.fromInterleavedDoubles(topology, frame, points))
    val fixed = CoordinateSmoothing.prepare(topology)(SmoothingBoundary.Fixed)
    val once = right(fixed.smooth(surface, parameters(1, 0.5)))
    val twice = right(fixed.smooth(surface, parameters(2, 0.5)))
    assertEquals(
      once.positions.interleavedDoubles.toVector,
      points.updated(2, 1.0).toVector
    )
    assertEquals(
      twice.positions.interleavedDoubles.toVector,
      points.updated(2, 0.5).toVector
    )
    val constrained = CoordinateSmoothing.prepare(topology)(
      SmoothingBoundary.Fixed,
      Iterator(topology.vertices.indexAtValidatedOrdinal(0))
    )
    assertEquals(
      right(
        constrained.smooth(surface, parameters(2))
      ).positions.interleavedDoubles.toVector,
      points.toVector
    )
    val free = CoordinateSmoothing.prepare(topology)(SmoothingBoundary.Free)
    val one = right(free.smooth(surface, parameters(1, 0.5))).positions.interleavedDoubles
    val two = right(free.smooth(surface, parameters(2, 0.5))).positions.interleavedDoubles
    close(one.take(6), Array(0.0, 0.0, 1.0, -0.5, -0.5, 1.0 / 3.0))
    close(two.take(6), Array(0.0, 0.0, 2.0 / 3.0, -0.25, -0.25, 4.0 / 9.0))

  test(
    "identity, exact ownership, input and scientific field immutability, float promotion"
  ):
    val topology = right(TriangleTopology.fromOrdinalFaces(5, faces))
    val input = points.map(_.toFloat)
    val field = right(PointField.fromInterleavedFloats(topology.vertices, frame, input))
    val surface = right(SurfaceRealization.fromPointField(topology, field))
    val scientific = VectorField.tabulate(topology.vertices)(_.ordinal.toDouble)
    val beforeFaces = topology.connectivityFingerprint
    val plan = CoordinateSmoothing.prepare(topology)(SmoothingBoundary.Free)
    assert(right(plan.smooth(surface, parameters(0))) eq surface)
    assert(right(plan.smooth(surface, parameters(4, 0.0))) eq surface)
    val output = right(plan.smooth(surface, parameters(2)))
    assert(output.topology eq topology)
    assert(output.frame eq frame)
    assert(output.positions.space.sameRuntimeOwnerAs(topology.vertices))
    assertEquals(output.positions.precision, PointStoragePrecision.Float64)
    assertEquals(topology.connectivityFingerprint, beforeFaces)
    assertEquals(input.toVector, points.map(_.toFloat).toVector)
    assertEquals(surface.positions.interleavedDoubles.toVector, points.toVector)
    assertEquals(scientific.toVector, Vector.tabulate(5)(_.toDouble))
    val escaped = output.positions.interleavedDoubles
    escaped(0) = 999.0
    assert(output.positions.interleavedDoubles(0) != 999.0)
    close(
      right(plan.smooth(surface, parameters(2))).positions.interleavedDoubles,
      output.positions.interleavedDoubles
    )

  test(
    "independent face-set reference, rigid transforms, scales and consistent relabeling"
  ):
    val topology = right(TriangleTopology.fromOrdinalFaces(5, faces))
    def run(p: Array[Double]) =
      val surface = right(SurfaceRealization.fromInterleavedDoubles(topology, frame, p))
      right(
        CoordinateSmoothing
          .prepare(topology)(SmoothingBoundary.Free)
          .smooth(surface, parameters(2, 0.3))
      ).positions.interleavedDoubles
    val expected = reference(points, faces, 2, 0.3)
    close(run(points), expected)
    def transform(p: Array[Double], scale: Double): Array[Double] =
      p.grouped(3)
        .flatMap(v =>
          Array(
            scale * (-0.8 * v(1) + 0.6 * v(0)) + 10.0,
            scale * (0.6 * v(1) + 0.8 * v(0)) - 20.0,
            scale * v(2) + 30.0
          )
        )
        .toArray
    List(1e-5, 1.0, 1e5).foreach: scale =>
      close(run(transform(points, scale)), transform(expected, scale), 1e-10)
    val permutation = Vector(4, 2, 0, 1, 3)
    val shuffledFaces = faces.reverse.map(t =>
      Triangle(permutation(t.third), permutation(t.first), permutation(t.second))
    )
    val shuffled = new Array[Double](points.length)
    permutation.indices.foreach(i =>
      Array.copy(points, i * 3, shuffled, permutation(i) * 3, 3)
    )
    val relabeled = right(TriangleTopology.fromOrdinalFaces(5, shuffledFaces))
    val surface =
      right(SurfaceRealization.fromInterleavedDoubles(relabeled, frame, shuffled))
    val actual = right(
      CoordinateSmoothing
        .prepare(relabeled)(SmoothingBoundary.Free)
        .smooth(surface, parameters(2, 0.3))
    ).positions.interleavedDoubles
    permutation.indices.foreach(i =>
      close(
        actual.slice(permutation(i) * 3, permutation(i) * 3 + 3),
        expected.slice(i * 3, i * 3 + 3)
      )
    )

  test("isolated vertices remain rejected by topology admission"):
    val result = TriangleTopology.fromOrdinalFaces(6, faces)
    assert(
      result.left.toOption.exists(_.issues.contains(mesh4s.TopologyIssue.UnusedVertex(5)))
    )

  test("constraints override free boundary and snapshot caller collections"):
    val topology = right(TriangleTopology.fromOrdinalFaces(5, faces))
    val surface = right(SurfaceRealization.fromInterleavedDoubles(topology, frame, points))
    val constraints =
      scala.collection.mutable.ArrayBuffer(topology.vertices.indexAtValidatedOrdinal(1))
    val plan = CoordinateSmoothing.prepare(topology)(SmoothingBoundary.Free, constraints)
    constraints.clear()
    val result = right(plan.smooth(surface, parameters(2))).positions.interleavedDoubles
    close(result.slice(3, 6), points.slice(3, 6))
    assert(result(2) < points(2))

  test("distinct owners cannot be smoothed even when types are widened"):
    val first = right(TriangleTopology.fromOrdinalFaces(5, faces))
    val second = right(TriangleTopology.fromOrdinalFaces(5, faces))
    val coordinates =
      right(PointField.fromInterleavedDoubles(second.vertices, frame, points))
    val surface =
      new SurfaceRealization[TriangleTopology, D3, Frame[D3]](second, coordinates)
    val plan = CoordinateSmoothing
      .prepare(first)(SmoothingBoundary.Free)

    assertEquals(
      plan.smooth(surface, parameters(0)).left.toOption,
      Some(SmoothingError.WrongTopology)
    )

  test("validated parameters, nonfinite admission and finite arithmetic refusal"):
    assertEquals(
      SmoothingParameters.create(-1, 0.1).left.toOption,
      Some(SmoothingError.InvalidIterations(-1))
    )
    List(-0.1, 1.1, Double.NaN, Double.PositiveInfinity).foreach(s =>
      assert(SmoothingParameters.create(1, s).isLeft)
    )
    assert(SmoothingParameters.create(1, 1.0).isRight)
    val topology = right(TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))))
    assert(
      SurfaceRealization
        .fromInterleavedDoubles(topology, frame, Array.fill(9)(Double.NaN))
        .isLeft
    )
    val extreme = right(
      SurfaceRealization.fromInterleavedDoubles(
        topology,
        frame,
        Array(Double.MaxValue, 0.0, 0.0, -Double.MaxValue, 0.0, 0.0, 0.0, 0.0, 0.0)
      )
    )
    assertEquals(
      CoordinateSmoothing
        .prepare(topology)(SmoothingBoundary.Free)
        .smooth(extreme, parameters(1))
        .left
        .toOption,
      Some(SmoothingError.NonFiniteUpdate(1, 0, 0))
    )

  test("empty topology, D2 degenerate geometry and collapsed output are explicit"):
    val empty = right(TriangleTopology.fromOrdinalFaces(0, Vector.empty))
    val emptySurface =
      right(SurfaceRealization.fromInterleavedDoubles(empty, frame, Array.emptyDoubleArray))
    assertEquals(
      right(
        CoordinateSmoothing
          .prepare(empty)(SmoothingBoundary.Free)
          .smooth(emptySurface, parameters(2))
      ).positions.interleavedDoubles.length,
      0
    )
    val topology = right(TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))))
    val frame2 = right(Frame.named[D2]("plane"))
    val line = right(
      SurfaceRealization.fromInterleavedDoubles(
        topology,
        frame2,
        Array(0.0, 0.0, 1.0, 0.0, 2.0, 0.0)
      )
    )
    val result = right(
      CoordinateSmoothing
        .prepare(topology)(SmoothingBoundary.Free)
        .smooth(line, parameters(1, 2.0 / 3.0))
    )
    close(result.positions.interleavedDoubles, Array(1.0, 0.0, 1.0, 0.0, 1.0, 0.0))
    assert(result.requireNondegenerate(NondegeneracyTolerance.scaleAware).isLeft)

  test("tetrahedron analytic shrinkage and displacement bound"):
    val tetraFaces =
      Vector(Triangle(0, 2, 1), Triangle(0, 1, 3), Triangle(0, 3, 2), Triangle(1, 2, 3))
    val topology = right(TriangleTopology.fromOrdinalFaces(4, tetraFaces))
    val p = Array(1.0, 1.0, 1.0, 1.0, -1.0, -1.0, -1.0, 1.0, -1.0, -1.0, -1.0, 1.0)
    val surface = right(SurfaceRealization.fromInterleavedDoubles(topology, frame, p))
    val result = right(
      CoordinateSmoothing
        .prepare(topology)(SmoothingBoundary.Free)
        .smooth(surface, parameters(2))
    )
    val factor = math.pow(1.0 - 4.0 * 0.1 / 3.0, 2)
    close(result.positions.interleavedDoubles, p.map(_ * factor))
    assertEqualsDouble(result.totalArea / surface.totalArea, factor * factor, 1e-12)
    assertEqualsDouble(
      SmoothingFixtures
        .measure(result.positions.interleavedDoubles, tetraFaces)
        .volume / SmoothingFixtures.measure(p, tetraFaces).volume,
      math.pow(factor, 3),
      1e-12
    )
    val displacement =
      p.zip(result.positions.interleavedDoubles).map((a, b) => math.abs(a - b)).max
    assert(displacement <= 2 * 0.1 * 2.0 + 1e-12)

  private def reference(
      p: Array[Double],
      triangles: Vector[Triangle[Int]],
      iterations: Int,
      step: Double
  ): Array[Double] =
    val adjacent = Vector.tabulate(p.length / 3): v =>
      triangles
        .flatMap(t => Vector(t.first, t.second, t.third))
        .distinct
        .filter: u =>
          u != v && triangles.exists(t =>
            Set(t.first, t.second, t.third).contains(v) && Set(t.first, t.second, t.third)
              .contains(u)
          )
    (0 until iterations).foldLeft(p): (old, _) =>
      Array.tabulate(p.length): i =>
        val row = adjacent(i / 3)
        if row.isEmpty then old(i)
        else (1 - step) * old(i) + step * row.map(v => old(v * 3 + i % 3)).sum / row.size

  private def close(
      actual: Array[Double],
      expected: Array[Double],
      tolerance: Double = 1e-12
  ): Unit =
    assertEquals(actual.length, expected.length)
    actual
      .zip(expected)
      .foreach((a, e) =>
        assert(math.abs(a - e) <= tolerance * math.max(1.0, math.abs(e)), s"$a != $e")
      )
