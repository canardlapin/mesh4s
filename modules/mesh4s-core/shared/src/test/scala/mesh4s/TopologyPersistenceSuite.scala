package mesh4s

import locus4s.DomainRecord

class TopologyPersistenceSuite extends munit.FunSuite:
  test("canonical fingerprint bytes have stable SHA-256 golden values"):
    assertEquals(
      TopologyFingerprint.compute(0, Vector.empty).value,
      "cbbc48750debb8535093b3deaf88ac7f4cff87425576a58de2bac754acdb4616"
    )
    assertEquals(
      TopologyFingerprint.compute(3, Vector(0, 1, 2)).value,
      "eca29f8938970a8a3546b717c6ed6dd97faf2e884ad22d27ca8944dfcfc73334"
    )
    assertEquals(
      TopologyFingerprint.compute(4, Vector(0, 1, 2, 0, 2, 3)).value,
      "80f069e67b780eedf230bea4f4313956c960c95578304f333e86cdbee1708234"
    )
    assertEquals(
      TopologyFingerprint.compute(16, torusOrdinals(4, 4)).value,
      "e4ffb6857c721879aa5df05cffe6dc4a01d0e8bca11647c28bcf922e585822a9"
    )

  test("fingerprints are structural rather than owner-bound"):
    val first = topology(Vector(Triangle(0, 1, 2)))
    val second = topology(Vector(Triangle(0, 1, 2)))
    assert(!(first eq second))
    assert(first.sameConnectivity(second))
    assertEquals(first.connectivityFingerprint, second.connectivityFingerprint)
    assertEquals(
      first.align(second),
      Left(TopologyAlignmentError.MissingPersistentRecord("left"))
    )

  test("records derive cell identities and reject altered fingerprints"):
    val record = triangleRecord("surface/a", Vector(0, 1, 2))
    assertEquals(record.edgeDomain.size, 3)
    assertEquals(record.faceDomain.size, 1)
    assertEquals(record.halfedgeDomain.size, 3)
    assert(record.edgeDomain.id.value.contains("surface/a:edge"))
    assert(record.faceDomain.id.value.contains("surface/a:face"))
    assert(record.halfedgeDomain.id.value.contains("surface/a:halfedge"))

    val altered = "0" * 64
    TriangleTopologyRecord.parse(
      record.topologyKey.value,
      record.vertexDomain,
      record.faceVertexOrdinals,
      record.indexingScheme.version,
      record.fingerprintScheme.version,
      altered
    ) match
      case Left(_: TopologyRecordError.FingerprintMismatch) => ()
      case other => fail(s"expected fingerprint mismatch, found $other")

  test("record parse is a stable round trip"):
    val record = triangleRecord("surface/round-trip", Vector(0, 1, 2))
    val parsed =
      TriangleTopologyRecord.parse(
        record.topologyKey.value,
        record.vertexDomain,
        record.faceVertexOrdinals,
        record.indexingScheme.version,
        record.fingerprintScheme.version,
        record.connectivityFingerprint.value
      )
    assertEquals(parsed, Right(record))

  test("one threaded registry canonicalizes restoration"):
    val record = triangleRecord("surface/canonical", Vector(0, 1, 2))
    val first = restore(TopologyRegistry.empty, record)
    val second = restore(first.registry, record)
    assert(first.topology eq second.topology)
    assertEquals(second.registry.size, 1)
    assertEquals(second.registry.domains.size, 4)
    assertEquals(second.topology.persistenceRecord, Some(record))

  test("a topology key rejects conflicting ordered incidence"):
    val firstRecord = triangleRecord("surface/conflict", Vector(0, 1, 2))
    val secondRecord = triangleRecord("surface/conflict", Vector(0, 2, 1))
    val first = restore(TopologyRegistry.empty, firstRecord)
    first.registry.restore(secondRecord) match
      case Left(_: TopologyRestoreError.ConflictingTopology) => ()
      case other => fail(s"expected topology conflict, found $other")

  test("independent registries require exact four-domain alignment"):
    val record = triangleRecord("surface/aligned", Vector(0, 1, 2))
    val leftResolution = restore(TopologyRegistry.empty, record)
    val rightResolution = restore(TopologyRegistry.empty, record)
    val left = leftResolution.topology
    val right = rightResolution.topology
    assert(!(left eq right))
    assert(!(left.vertices eq right.vertices))

    val alignment = rightOrFail(left.align(right))
    assertEquals(
      alignment.vertexToRight(index(left.vertices, 2)).ordinal,
      2
    )
    assertEquals(
      alignment.edgeToRight(index(left.edges, 1)).ordinal,
      1
    )
    assertEquals(
      alignment.faceToRight(index(left.faces, 0)).ordinal,
      0
    )
    assertEquals(
      alignment.halfedgeToRight(index(left.halfedges, 2)).ordinal,
      2
    )

  test("alignment identity, inverse, and composition preserve ordinals"):
    val record = triangleRecord("surface/groupoid", Vector(0, 1, 2))
    val leftResolution = restore(TopologyRegistry.empty, record)
    val middleResolution = restore(TopologyRegistry.empty, record)
    val rightResolution = restore(TopologyRegistry.empty, record)
    val left = leftResolution.topology
    val middle = middleResolution.topology
    val right = rightResolution.topology
    val identity = TopologyAlignment.identity(left)
    val leftMiddle = rightOrFail(left.align(middle))
    val middleRight = rightOrFail(middle.align(right))
    val composed = rightOrFail(leftMiddle.andThen(middleRight))
    val reversed = leftMiddle.reverse
    val vertex = index(left.vertices, 1)

    assertEquals(identity.vertexToRight(vertex).ordinal, vertex.ordinal)
    assertEquals(
      reversed
        .vertexToRight(index(reversed.left.vertices, vertex.ordinal))
        .ordinal,
      vertex.ordinal
    )
    assertEquals(composed.vertexToRight(vertex).ordinal, 1)

  test("same topology key with changed ordered incidence does not align"):
    val leftResolution =
      restore(
        TopologyRegistry.empty,
        triangleRecord("surface/order", Vector(0, 1, 2))
      )
    val rightResolution =
      restore(
        TopologyRegistry.empty,
        triangleRecord("surface/order", Vector(0, 2, 1))
      )
    val left = leftResolution.topology
    val right = rightResolution.topology
    assertEquals(
      left.align(right),
      Left(TopologyAlignmentError.DifferentIncidence)
    )

  private def topology(rows: Vector[Triangle[Int]]): TriangleTopology =
    TriangleTopology
      .fromOrdinalFaces(3, rows)
      .fold(audit => fail(audit.toString), identity)

  private def triangleRecord(
      topologyId: String,
      ordinals: Vector[Int]
  ): TriangleTopologyRecord =
    val vertexDomain =
      DomainRecord
        .parse(
          s"$topologyId/vertices",
          "surface vertices",
          3,
          Some("fixture-vertices-v1")
        )
        .fold(error => fail(error.message), identity)
    val key =
      TopologyKey.parse(topologyId).fold(error => fail(error.message), identity)
    TriangleTopologyRecord
      .create(key, vertexDomain, ordinals)
      .fold(error => fail(error.message), identity)

  private def restore(
      registry: TopologyRegistry,
      record: TriangleTopologyRecord
  ): TopologyResolution =
    registry.restore(record).fold(error => fail(error.message), identity)

  private def index[S](
      domain: locus4s.FiniteDomain[S],
      ordinal: Int
  ): locus4s.Index[S] =
    domain.index(ordinal).fold(error => fail(error.message), identity)

  private def rightOrFail[A](
      value: Either[TopologyAlignmentError, A]
  ): A =
    value.fold(error => fail(error.message), identity)

  private def torusOrdinals(rows: Int, columns: Int): Vector[Int] =
    def vertex(row: Int, column: Int): Int =
      (row % rows) * columns + column % columns

    Vector
      .tabulate(rows, columns) { (row, column) =>
        val first = vertex(row, column)
        val below = vertex(row + 1, column)
        val diagonal = vertex(row + 1, column + 1)
        val right = vertex(row, column + 1)
        Vector(first, below, diagonal, first, diagonal, right)
      }
      .flatten
      .flatten
