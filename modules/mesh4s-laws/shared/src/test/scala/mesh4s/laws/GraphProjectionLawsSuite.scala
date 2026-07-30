package mesh4s.laws

import mesh4s.TriangleTable
import mesh4s.TriangleTopology
import mesh4s.geometry.PiecewiseEuclideanMetric
import mesh4s.graph4s.GraphProjections
import org.scalacheck.Prop.forAll
import spatial4s.CoordinateUnit

class GraphProjectionLawsSuite extends munit.ScalaCheckSuite:
  property("the primal graph has total vertex and edge correspondences"):
    forAll(TopologyFixtures.validGenerator): table =>
      val topology = build(table)
      val primal =
        GraphProjections.primal(topology).fold(error => fail(error.message), identity)

      assertEquals(primal.graph.vertexCount, topology.vertices.size)
      assertEquals(primal.graph.edgeCount, topology.edges.size)

      topology.vertices.foreachIndex: vertex =>
        val graphVertex = primal.graphVertex(vertex)
        assertEquals(primal.meshVertex(graphVertex), vertex)
        val expected =
          topology.neighbors(vertex).ordinalsInDomainOrder.toSet
        val actual =
          primal.graph
            .neighbors(graphVertex)
            .iterator
            .map(primal.meshVertex)
            .map(_.ordinal)
            .toSet
        assertEquals(actual, expected)

      topology.edges.foreachIndex: edge =>
        val graphEdge = primal.graphEdge(edge)
        assertEquals(primal.meshEdge(graphEdge), edge)
        val meshEndpoints = topology.endpointsOf(edge)
        val (firstGraphVertex, secondGraphVertex) =
          primal.graph.endpoints(graphEdge)
        assertEquals(
          Set(
            primal.meshVertex(firstGraphVertex).ordinal,
            primal.meshVertex(secondGraphVertex).ordinal
          ),
          Set(meshEndpoints.first.ordinal, meshEndpoints.second.ordinal)
        )

  property("the interior dual forgets exactly the boundary mesh edges"):
    forAll(TopologyFixtures.validGenerator): table =>
      val topology = build(table)
      val dual =
        GraphProjections
          .interiorDual(topology)
          .fold(error => fail(error.message), identity)

      assertEquals(dual.graph.vertexCount, topology.faces.size)
      assertEquals(
        dual.graph.edgeCount,
        topology.edges.size - topology.boundaryHalfedgeCount
      )

      topology.faces.foreachIndex: face =>
        assertEquals(dual.meshFace(dual.graphVertex(face)), face)

      topology.edges.foreachIndex: edge =>
        val isInterior = edgeIsInterior(topology, edge)
        assertEquals(dual.graphEdge(edge).nonEmpty, isInterior)
        dual
          .graphEdge(edge)
          .foreach: graphEdge =>
            assertEquals(dual.meshInteriorEdge(graphEdge), edge)

  property("metric weighting is explicit and preserves each mesh edge length"):
    forAll(TopologyFixtures.validGenerator): table =>
      val topology = build(table)
      val metric =
        PiecewiseEuclideanMetric
          .fromValues(
            topology,
            CoordinateUnit.Millimeter,
            Vector.fill(topology.edges.size)(1.0)
          )
          .fold(audit => fail(audit.message), identity)
      val weighted =
        GraphProjections
          .metricWeightedPrimal(topology, metric)
          .fold(error => fail(error.message), identity)

      weighted.primal.graph.edges.foreach: graphEdge =>
        val meshEdge = weighted.primal.meshEdge(graphEdge)
        assertEqualsDouble(
          weighted.weight(graphEdge),
          metric.length(
            metric.topology.edges.indexAtValidatedOrdinal(meshEdge.ordinal)
          ),
          0.0
        )

  test("topology-only projections require no realization or metric"):
    val topology = build(TopologyFixtures.disk)
    val primal = GraphProjections.primal(topology)
    val dual = GraphProjections.interiorDual(topology)
    assert(primal.isRight)
    assert(dual.isRight)

  private def build(table: TriangleTable): TriangleTopology =
    TriangleTopology
      .fromTable(table)
      .fold(audit => fail(audit.message), identity)

  private def edgeIsInterior(
      topology: TriangleTopology,
      edge: topology.EdgeIndex
  ): Boolean =
    var interior = false
    topology.halfedges.foreachIndex: halfedge =>
      if topology.edgeOf(halfedge) == edge then
        interior = topology.opposite(halfedge).nonEmpty
    interior
