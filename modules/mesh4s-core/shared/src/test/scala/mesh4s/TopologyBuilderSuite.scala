package mesh4s

import locus4s.Index

class TopologyBuilderSuite extends munit.FunSuite:
  test("single triangle compiles to one face-corner halfedge row"):
    val topology =
      right(
        TriangleTopology.fromOrdinalFaces(
          3,
          Vector(Triangle(0, 1, 2))
        )
      )

    assertEquals(topology.vertices.size, 3)
    assertEquals(topology.edges.size, 3)
    assertEquals(topology.faces.size, 1)
    assertEquals(topology.halfedges.size, 3)
    assertEquals(topology.boundaryHalfedgeCount, 3)

    val h0 = index(topology.halfedges, 0)
    assertEquals(topology.origin(h0).ordinal, 0)
    assertEquals(topology.target(h0).ordinal, 1)
    assertEquals(topology.next(h0).ordinal, 1)
    assertEquals(topology.previous(h0).ordinal, 2)
    assertEquals(topology.opposite(h0), None)

  test("typed construction preserves an existing vertex domain"):
    val packed =
      locus4s.FiniteDomain
        .ephemeral("existing surface vertices", 3)
        .fold(error => fail(error.message), identity)
    val vertices = packed.value
    val v0 = index(vertices, 0)
    val v1 = index(vertices, 1)
    val v2 = index(vertices, 2)
    val topology =
      right(
        TriangleTopology
          .on(vertices)
          .fromOrientedFaces(Vector(Triangle(v0, v1, v2)))
      )

    assert(topology.vertices eq vertices)
    assertEquals(
      topology.verticesOf(index(topology.faces, 0)),
      Triangle(v0, v1, v2)
    )

  test("two oriented triangles pair one shared edge deterministically"):
    val topology = square()
    assertEquals(topology.edges.size, 5)
    assertEquals(topology.halfedges.size, 6)
    assertEquals(topology.boundaryHalfedgeCount, 4)

    val sharedLeft = index(topology.halfedges, 2)
    val sharedRight = index(topology.halfedges, 3)
    assertEquals(topology.opposite(sharedLeft).map(_.ordinal), Some(3))
    assertEquals(topology.opposite(sharedRight).map(_.ordinal), Some(2))
    assertEquals(
      topology.edgeOf(sharedLeft).ordinal,
      topology.edgeOf(sharedRight).ordinal
    )

    val vertex0 = index(topology.vertices, 0)
    assertEquals(
      topology.neighbors(vertex0).ordinalsInDomainOrder.toVector,
      Vector(1, 2, 3)
    )
    assertEquals(
      topology.incidentFaces(vertex0).ordinalsInDomainOrder.toVector,
      Vector(0, 1)
    )

  test("closed tetrahedron satisfies face-cycle and opposite laws"):
    val topology =
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
    assert(topology.isClosed)
    assertEquals(topology.edges.size, 6)
    topology.halfedges.foreachIndex { halfedge =>
      assertEquals(
        topology.next(topology.next(topology.next(halfedge))).ordinal,
        halfedge.ordinal
      )
      val opposite = topology.opposite(halfedge).getOrElse(fail("closed edge"))
      assertEquals(
        topology.opposite(opposite).map(_.ordinal),
        Some(halfedge.ordinal)
      )
      assertEquals(topology.origin(opposite), topology.target(halfedge))
      assertEquals(topology.target(opposite), topology.origin(halfedge))
    }

  test("strict construction accumulates local input witnesses"):
    val audit =
      left(
        TriangleTopology.fromOrdinalFaces(
          4,
          Vector(
            Triangle(0, 0, 8),
            Triangle(0, 1, 2),
            Triangle(2, 1, 0)
          )
        )
      )
    assert(audit.issues.exists {
      case TopologyIssue.VertexOutOfBounds(0, 2, 8) => true
      case _                                        => false
    })
    assert(audit.issues.exists {
      case TopologyIssue.RepeatedVertex(0, 0) => true
      case _                                  => false
    })
    assert(audit.issues.exists {
      case TopologyIssue.DuplicateFace(1, 2) => true
      case _                                 => false
    })
    assert(audit.issues.exists {
      case TopologyIssue.UnusedVertex(3) => true
      case _                             => false
    })

  test("same-direction shared edges are rejected without repair"):
    val audit =
      left(
        TriangleTopology.fromOrdinalFaces(
          4,
          Vector(
            Triangle(0, 1, 2),
            Triangle(0, 1, 3)
          )
        )
      )
    assert(audit.issues.exists {
      case TopologyIssue.OrientationConflict(0, 1, 0, 1) => true
      case _                                             => false
    })

  test("three-face edges and bow-tie vertices are rejected"):
    val edgeAudit =
      left(
        TriangleTopology.fromOrdinalFaces(
          5,
          Vector(
            Triangle(0, 1, 2),
            Triangle(1, 0, 3),
            Triangle(0, 1, 4)
          )
        )
      )
    assert(edgeAudit.issues.exists {
      case TopologyIssue.NonManifoldEdge(0, 1, faces) =>
        faces == Vector(0, 1, 2)
      case _ => false
    })

    val vertexAudit =
      left(
        TriangleTopology.fromOrdinalFaces(
          5,
          Vector(
            Triangle(0, 1, 2),
            Triangle(0, 3, 4)
          )
        )
      )
    assert(vertexAudit.issues.exists {
      case TopologyIssue.NonManifoldVertex(0, 2, _) => true
      case _                                        => false
    })

  test("empty topology is admitted and nonempty unused domains are rejected"):
    val empty = right(TriangleTopology.fromOrdinalFaces(0, Vector.empty))
    assertEquals(empty.vertices.size, 0)
    assertEquals(empty.faces.size, 0)
    assert(empty.isClosed)

    val audit =
      left(TriangleTopology.fromOrdinalFaces(2, Vector.empty))
    assertEquals(
      audit.issues.collect { case TopologyIssue.UnusedVertex(vertex) => vertex },
      Vector(0, 1)
    )

  test("bounded audits retain the total issue count"):
    val audit =
      left(
        TriangleTopology.fromOrdinalFaces(
          10,
          Vector.empty,
          issueLimit = 3
        )
      )
    assertEquals(audit.issues.length, 3)
    assertEquals(audit.totalIssueCount, 10L)
    assert(audit.truncated)

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

  private def index[S](domain: locus4s.FiniteDomain[S], ordinal: Int): Index[S] =
    domain.index(ordinal).fold(error => fail(error.message), identity)

  private def right[A](value: Either[TopologyAudit, A]): A =
    value.fold(audit => fail(audit.message), identity)

  private def left[A](value: Either[TopologyAudit, A]): TopologyAudit =
    value.fold(identity, _ => fail("expected topology construction to fail"))
