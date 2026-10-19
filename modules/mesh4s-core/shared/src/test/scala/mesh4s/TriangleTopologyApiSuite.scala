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

  test("an exact alignment provides concise typed transport"):
    val errors = typeCheckErrors(
      """import mesh4s.TopologyAlignment
import mesh4s.TriangleTopology

def transport(left: TriangleTopology, right: TriangleTopology)(
    alignment: TopologyAlignment[left.type, right.type],
    vertex: left.VertexIndex
): right.VertexIndex =
  alignment.vertexToRight(vertex)
"""
    )

    assertEquals(errors, Nil)

  test("an image-grid-like voxel domain cannot be used as mesh vertices"):
    val errors = typeCheckErrors(
      """import locus4s.Index
import mesh4s.TriangleTopology

trait ImageGrid:
  type Voxel

def misuse(topology: TriangleTopology, grid: ImageGrid)(
    voxel: Index[grid.Voxel]
) =
  topology.neighbors(voxel)
"""
    )

    assert(errors.nonEmpty)

  test("packed faces expose the admitted triangle rows without copying"):
    val rows = Vector(Triangle(0, 1, 2), Triangle(0, 2, 3))
    val topology =
      TriangleTopology
        .fromOrdinalFaces(4, rows)
        .fold(audit => fail(audit.message), identity)
    val packed = topology.packedFaces
    assertEquals(packed.toVector, rows.flatMap(_.toVector))
    assert(packed.asInstanceOf[AnyRef] eq topology.packedFaces.asInstanceOf[AnyRef])
    val same =
      TriangleTopology
        .fromOrdinalFaces(4, rows)
        .fold(audit => fail(audit.message), identity)
    assert(topology.sameConnectivity(same))
    assertEquals(topology.connectivityFingerprint, same.connectivityFingerprint)
