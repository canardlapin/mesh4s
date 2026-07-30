package mesh4s.laws

import locus4s.DomainRecord
import mesh4s.TopologyAlignmentError
import mesh4s.TopologyKey
import mesh4s.TopologyRecordError
import mesh4s.TopologyRegistry
import mesh4s.TopologyResolution
import mesh4s.TriangleTopologyRecord
import org.scalacheck.Prop.forAll

class TopologyPersistenceLawsSuite extends munit.ScalaCheckSuite:
  property("record restoration is stable and canonical"):
    forAll(TopologyFixtures.validGenerator) { table =>
      val record = persistenceRecord("canonical", table)
      val first = restore(TopologyRegistry.empty, record)
      val second = restore(first.registry, record)
      assert(first.topology eq second.topology)
      assertEquals(second.topology.persistenceRecord, Some(record))
      assertEquals(
        second.topology.connectivityFingerprint,
        record.connectivityFingerprint
      )
    }

  property("independent restoration aligns every cell domain by ordinal"):
    forAll(TopologyFixtures.validGenerator) { table =>
      val record = persistenceRecord("alignment", table)
      val leftResolution = restore(TopologyRegistry.empty, record)
      val rightResolution = restore(TopologyRegistry.empty, record)
      val left = leftResolution.topology
      val right = rightResolution.topology
      val alignment =
        left.align(right).fold(error => fail(error.message), identity)

      left.vertices.foreachIndex(vertex =>
        assertEquals(alignment.vertexToRight(vertex).ordinal, vertex.ordinal)
      )
      left.edges.foreachIndex(edge =>
        assertEquals(alignment.edgeToRight(edge).ordinal, edge.ordinal)
      )
      left.faces.foreachIndex(face =>
        assertEquals(alignment.faceToRight(face).ordinal, face.ordinal)
      )
      left.halfedges.foreachIndex(halfedge =>
        assertEquals(
          alignment.halfedgeToRight(halfedge).ordinal,
          halfedge.ordinal
        )
      )
    }

  property("alignment composition agrees with direct alignment"):
    forAll(TopologyFixtures.validGenerator) { table =>
      val record = persistenceRecord("groupoid", table)
      val leftResolution = restore(TopologyRegistry.empty, record)
      val middleResolution = restore(TopologyRegistry.empty, record)
      val rightResolution = restore(TopologyRegistry.empty, record)
      val left = leftResolution.topology
      val middle = middleResolution.topology
      val right = rightResolution.topology
      val leftMiddle =
        left.align(middle).fold(error => fail(error.message), identity)
      val middleRight =
        middle.align(right).fold(error => fail(error.message), identity)
      val direct =
        left.align(right).fold(error => fail(error.message), identity)
      val composed =
        leftMiddle
          .andThen(middleRight)
          .fold(error => fail(error.message), identity)

      left.vertices.foreachIndex { vertex =>
        assertEquals(
          composed.vertexToRight(vertex).ordinal,
          direct.vertexToRight(vertex).ordinal
        )
      }
    }

  property("alignment composition is associative"):
    forAll(TopologyFixtures.validGenerator) { table =>
      val record = persistenceRecord("associativity", table)
      val firstResolution = restore(TopologyRegistry.empty, record)
      val secondResolution = restore(TopologyRegistry.empty, record)
      val thirdResolution = restore(TopologyRegistry.empty, record)
      val fourthResolution = restore(TopologyRegistry.empty, record)
      val first = firstResolution.topology
      val second = secondResolution.topology
      val third = thirdResolution.topology
      val fourth = fourthResolution.topology
      val firstSecond =
        first.align(second).fold(error => fail(error.message), identity)
      val secondThird =
        second.align(third).fold(error => fail(error.message), identity)
      val thirdFourth =
        third.align(fourth).fold(error => fail(error.message), identity)
      val leftAssociated =
        firstSecond
          .andThen(secondThird)
          .flatMap(_.andThen(thirdFourth))
          .fold(error => fail(error.message), identity)
      val rightAssociated =
        secondThird
          .andThen(thirdFourth)
          .flatMap(firstSecond.andThen)
          .fold(error => fail(error.message), identity)

      first.vertices.foreachIndex { vertex =>
        assertEquals(
          leftAssociated.vertexToRight(vertex).ordinal,
          rightAssociated.vertexToRight(vertex).ordinal
        )
      }
    }

  property("matching structural fingerprints do not authorize transport"):
    forAll(TopologyFixtures.validGenerator) { table =>
      val firstRecord = persistenceRecord("authority-a", table)
      val secondRecord = persistenceRecord("authority-b", table)
      assertEquals(
        firstRecord.connectivityFingerprint,
        secondRecord.connectivityFingerprint
      )
      val firstResolution = restore(TopologyRegistry.empty, firstRecord)
      val secondResolution = restore(TopologyRegistry.empty, secondRecord)
      firstResolution.topology.align(secondResolution.topology) match
        case Left(_: TopologyAlignmentError.DifferentTopologyKeys) => ()
        case other => fail(s"expected different topology keys, found $other")
    }

  private def persistenceRecord(
      suffix: String,
      table: mesh4s.TriangleTable
  ): TriangleTopologyRecord =
    val structural =
      mesh4s.TopologyFingerprint
        .compute(
          table.vertexCount,
          table.faces.iterator.flatMap(_.toVector)
        )
        .value
    val vertexRecord =
      DomainRecord
        .parse(
          s"fixture/$suffix/$structural/vertices",
          "generated vertices",
          table.vertexCount,
          Some(s"fixture:$structural")
        )
        .fold(error => fail(error.message), identity)
    val topologyKey =
      TopologyKey
        .parse(s"fixture/$suffix/$structural")
        .fold(error => fail(error.message), identity)
    TriangleTopologyRecord
      .create(
        topologyKey,
        vertexRecord,
        table.faces.iterator.flatMap(_.toVector)
      )
      .fold(recordFailure, identity)

  private def recordFailure(error: TopologyRecordError): Nothing =
    fail(error.message)

  private def restore(
      registry: TopologyRegistry,
      record: TriangleTopologyRecord
  ): TopologyResolution =
    registry.restore(record).fold(error => fail(error.message), identity)
