package mesh4s

import scala.compiletime.testing.typeCheckErrors

class TriangleTopologyApiSuite extends munit.FunSuite:
  test("ordinary code can use topology-local aliases"):
    val errors = typeCheckErrors(
      """import mesh4s.TriangleTopology

def originOf(topology: TriangleTopology)(
    halfedge: topology.HalfedgeIndex
): topology.VertexIndex =
  topology.origin(halfedge)
"""
    )

    assertEquals(errors, Nil)

  test("a face cannot be passed to a halfedge operation"):
    val errors = typeCheckErrors(
      """import mesh4s.TriangleTopology

def misuse(topology: TriangleTopology)(
    face: topology.FaceIndex
): topology.VertexIndex =
  topology.origin(face)
"""
    )

    assert(errors.nonEmpty)

  test("indices from distinct topology owners cannot be mixed"):
    val errors = typeCheckErrors(
      """import mesh4s.TriangleTopology

def mix(left: TriangleTopology, right: TriangleTopology)(
    vertex: left.VertexIndex
): Boolean =
  right.vertices.contains(vertex)
"""
    )

    assert(errors.nonEmpty)
