package mesh4s

import locus4s.Index

import scala.collection.mutable

class TopologyTraversalSuite extends munit.FunSuite:
  test("boundary vertex fans preserve directed path semantics"):
    val topology = square()
    val vertex = index(topology.vertices, 0)
    topology.fan(vertex) match
      case VertexFan.Boundary(halfedges, faces, neighbors) =>
        assertEquals(halfedges.values.map(_.ordinal), Vector(0, 3))
        assertEquals(faces.values.map(_.ordinal), Vector(0, 1))
        assertEquals(neighbors.values.map(_.ordinal), Vector(1, 2, 3))
      case _: VertexFan.Interior[?, ?, ?] =>
        fail("square corner must have a boundary path")

  test("interior vertex fans are cycles with rotation-invariant equality"):
    val topology = tetrahedron()
    val vertex = index(topology.vertices, 0)
    topology.fan(vertex) match
      case VertexFan.Interior(halfedges, faces, neighbors) =>
        assertEquals(halfedges.size, 3)
        assertEquals(faces.size, 3)
        assertEquals(neighbors.size, 3)
        assertEquals(
          Cycle.fromVector(neighbors.values.tail :+ neighbors.values.head),
          Some(neighbors)
        )
      case _: VertexFan.Boundary[?, ?, ?] =>
        fail("closed tetrahedron vertex must have a cyclic fan")

  test("boundary loops are directed, deterministic, disjoint, and covering"):
    val topology = square()
    val loops = topology.boundaryLoops
    assertEquals(loops.length, 1)
    assertEquals(loops.head.values.map(_.ordinal), Vector(0, 1, 4, 5))

    val traversed = loops.flatMap(_.values.map(_.ordinal)).sorted
    val primitive = Vector.newBuilder[Int]
    topology.foreachBoundaryHalfedge(halfedge => primitive += halfedge.ordinal)
    assertEquals(traversed, primitive.result().sorted)
    assertEquals(traversed.distinct, traversed)

  test("components and summaries are deterministic"):
    val topology =
      right(
        TriangleTopology.fromOrdinalFaces(
          6,
          Vector(
            Triangle(0, 1, 2),
            Triangle(3, 4, 5)
          )
        )
      )
    assertEquals(
      topology.vertexComponents.map(_.ordinalsInDomainOrder.toVector),
      Vector(Vector(0, 1, 2), Vector(3, 4, 5))
    )
    assertEquals(topology.componentCount, 2)
    assertEquals(topology.boundaryLoops.length, 2)
    assertEquals(topology.eulerCharacteristic, 2)
    assert(topology.requireConnected.isLeft)
    assert(topology.requireClosed.isLeft)

    val empty = right(TriangleTopology.fromOrdinalFaces(0, Vector.empty))
    assertEquals(empty.componentCount, 0)
    assert(empty.requireConnected.isRight)
    assert(empty.requireClosed.isRight)

  test("edge and face incidence form an exact chain complex"):
    val topologies = Vector(square(), tetrahedron())
    topologies.foreach { topology =>
      topology.faces.foreachIndex { face =>
        val coefficients =
          mutable.HashMap.empty[Int, Int].withDefaultValue(0)
        topology.chains.foreachFaceBoundary(face) { (edge, faceCoefficient) =>
          topology.chains.foreachEdgeBoundary(edge) { (vertex, edgeCoefficient) =>
            val ordinal = vertex.ordinal
            coefficients.update(
              ordinal,
              coefficients(ordinal) + faceCoefficient * edgeCoefficient
            )
          }
        }
        assert(
          coefficients.valuesIterator.forall(_ == 0),
          s"boundary1(boundary2(face ${face.ordinal})) = $coefficients"
        )
      }
    }

  test("face and boundary callbacks emit the expected primitive cells"):
    val topology = square()
    val faceHalfedges = Vector.newBuilder[Int]
    topology.foreachFaceHalfedge(index(topology.faces, 1)) { halfedge =>
      faceHalfedges += halfedge.ordinal
    }
    assertEquals(faceHalfedges.result(), Vector(3, 4, 5))

    topology.edges.foreachIndex { edge =>
      val endpoints = topology.endpointsOf(edge)
      assert(endpoints.first.ordinal < endpoints.second.ordinal)
    }

  test("audit stages do not overclaim after malformed local rows"):
    val audit =
      left(
        TriangleTopology.fromOrdinalFaces(
          3,
          Vector(Triangle(0, 0, 8))
        )
      )
    assert(audit.completedStages.contains(AuditStage.Addressability))
    assert(audit.completedStages.contains(AuditStage.LocalFaces))
    assert(!audit.completedStages.contains(AuditStage.DuplicateFaces))
    assert(!audit.completedStages.contains(AuditStage.EdgeIncidence))
    assert(!audit.completedStages.contains(AuditStage.VertexLinks))

  private def square(): TriangleTopology =
    right(
      TriangleTopology.fromOrdinalFaces(
        4,
        Vector(
          Triangle(0, 1, 2),
          Triangle(0, 2, 3)
        )
      )
    )

  private def tetrahedron(): TriangleTopology =
    right(
      TriangleTopology.fromOrdinalFaces(
        4,
        Vector(
          Triangle(0, 2, 1),
          Triangle(0, 1, 3),
          Triangle(1, 2, 3),
          Triangle(2, 0, 3)
        )
      )
    )

  private def index[S](domain: locus4s.FiniteDomain[S], ordinal: Int): Index[S] =
    domain.index(ordinal).fold(error => fail(error.message), identity)

  private def right[A](value: Either[TopologyAudit, A]): A =
    value.fold(audit => fail(audit.message), identity)

  private def left[A](value: Either[TopologyAudit, A]): TopologyAudit =
    value.fold(identity, _ => fail("expected topology construction to fail"))
