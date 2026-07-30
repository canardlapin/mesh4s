package mesh4s

final class PrimitiveTraversalChecksumSuite extends munit.FunSuite:
  test("primitive traversal checksum is deterministic across runtimes"):
    val topology =
      TriangleTopology
        .fromOrdinalFaces(
          4,
          Vector(
            Triangle(0, 1, 2),
            Triangle(0, 2, 3)
          )
        )
        .fold(audit => fail(audit.message), identity)
    var checksum = 17L
    def mix(value: Int): Unit =
      checksum = checksum * 31L + value.toLong

    topology.vertices.foreachIndex: vertex =>
      topology.foreachNeighbor(vertex): neighbor =>
        mix(vertex.ordinal * 11 + neighbor.ordinal)

    topology.vertices.foreachIndex: vertex =>
      topology.foreachIncidentFace(vertex): face =>
        mix(vertex.ordinal * 17 + face.ordinal)

    topology.faces.foreachIndex: face =>
      topology.foreachFaceHalfedge(face): halfedge =>
        mix(face.ordinal * 19 + halfedge.ordinal)

    topology.foreachBoundaryHalfedge: halfedge =>
      mix(halfedge.ordinal)

    assertEquals(checksum, -712911023278697694L)
