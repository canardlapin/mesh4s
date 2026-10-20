package mesh4s.graph4s

import locus4s.data.VectorField
import mesh4s.{Triangle, TriangleTopology}

class MeshShortestPathsSuite extends munit.FunSuite:
  private def ok[E, A](value: Either[E, A]): A = value.fold(e => fail(s"$e"), identity)

  test(
    "typed mesh costs match independent Floyd-Warshall without an intrinsic metric constraint"
  ):
    val topology = ok(
      TriangleTopology.fromOrdinalFaces(
        7,
        Vector(Triangle(0, 1, 2), Triangle(1, 3, 2), Triangle(4, 5, 6))
      )
    )
    (0 until 24).foreach: seed =>
      val weights = Vector.tabulate(topology.edges.size)(i =>
        if (i + seed) % 4 == 0 then 0.0 else ((7 * i + seed) % 9 + 1) * 0.25
      )
      val field = ok(VectorField.fromValues(topology.edges, weights))
      val prepared = ok(MeshShortestPaths.create(topology)(field))
      assert(prepared.topology eq topology)
      val expected = Array.fill(7, 7)(Double.PositiveInfinity)
      (0 until 7).foreach(i => expected(i)(i) = 0)
      // Independent endpoint matrix and all-pairs dynamic program.
      topology.edges.foreachIndex: edge =>
        val pair = topology.endpointsOf(edge)
        expected(pair.first.ordinal)(pair.second.ordinal) = field(edge)
        expected(pair.second.ordinal)(pair.first.ordinal) = field(edge)
      for k <- 0 until 7; i <- 0 until 7; j <- 0 until 7 do
        expected(i)(j) = math.min(expected(i)(j), expected(i)(k) + expected(k)(j))
      topology.vertices.foreachIndex: source =>
        val distances = prepared.distancesFrom(source)
        assert(distances.vertices.sameRuntimeOwnerAs(topology.vertices))
        topology.vertices.foreachIndex: target =>
          if expected(source.ordinal)(target.ordinal).isPosInfinity then
            assertEquals(distances.distanceTo(target), None)
          else
            assertEqualsDouble(
              distances.distanceTo(target).get,
              expected(source.ordinal)(target.ordinal),
              0.0
            )

  test(
    "invalid costs refuse explicitly and zero costs do not imply invalid triangle lengths"
  ):
    val topology = ok(TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))))
    Vector(-1.0, Double.NaN, Double.PositiveInfinity).foreach: cost =>
      val field = ok(VectorField.fromValues(topology.edges, Vector.fill(3)(cost)))
      assert(MeshShortestPaths.create(topology)(field).isLeft)
    val zeros = ok(VectorField.fromValues(topology.edges, Vector.fill(3)(0.0)))
    val prepared = ok(MeshShortestPaths.create(topology)(zeros))
    val result = prepared.distancesFrom(topology.vertices.indexAtValidatedOrdinal(0))
    assertEquals(result.distanceTo(topology.vertices.indexAtValidatedOrdinal(2)), Some(0.0))
    assertEquals(topology.packedFaces.toVector, Vector(0, 1, 2))

  test("another topology's vertex and edge domains cannot cross the adapter"):
    assert(compileErrors("""
      import mesh4s.TriangleTopology
      import mesh4s.graph4s.MeshShortestPaths
      import locus4s.data.Field
      def wrong(a: TriangleTopology, b: TriangleTopology)(costs: Field[b.Edge, Double]) =
        MeshShortestPaths.create(a)(costs)
    """).nonEmpty)
    assert(compileErrors("""
      import mesh4s.TriangleTopology
      import mesh4s.graph4s.MeshShortestPaths
      import locus4s.data.Field
      def wrong(a: TriangleTopology, b: TriangleTopology)(costs: Field[a.Edge, Double], vertex: locus4s.Index[b.Vertex]) =
        MeshShortestPaths.create(a)(costs).toOption.get.distancesFrom(vertex)
    """).nonEmpty)
