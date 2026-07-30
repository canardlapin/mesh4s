package mesh4s.laws

import mesh4s.OrientationError
import mesh4s.TopologyAudit
import mesh4s.TriangleTopology
import mesh4s.reference.ReferenceTopology
import org.scalacheck.Prop.forAll

class TopologyLawsSuite extends munit.ScalaCheckSuite:
  property("optimized and reference representations agree on valid fixtures"):
    forAll(TopologyFixtures.validGenerator) { table =>
      val topology = right(TriangleTopology.fromTable(table))
      val reference =
        ReferenceTopology
          .fromTable(table)
          .fold(issues => fail(issues.mkString("\n")), identity)

      assertEquals(topology.vertices.size, reference.vertexCount)
      assertEquals(topology.edges.size, reference.edgeCount)
      assertEquals(topology.faces.size, reference.faceCount)
      assertEquals(topology.halfedges.size, reference.halfedgeCount)
      assertEquals(topology.boundaryHalfedgeCount, reference.boundaryEdgeCount)
      (0 until topology.vertices.size).foreach { ordinal =>
        val vertex =
          topology.vertices
            .index(ordinal)
            .fold(error => fail(error.message), identity)
        assertEquals(
          topology.neighbors(vertex).ordinalsInDomainOrder.toSet,
          reference.neighbors(ordinal)
        )
        assertEquals(
          topology.incidentFaces(vertex).ordinalsInDomainOrder.toSet,
          reference.incidentFaces(ordinal)
        )
      }
    }

  property("strict and reference validation reject malformed fixtures"):
    forAll(TopologyFixtures.malformedGenerator) { table =>
      assert(TriangleTopology.fromTable(table).isLeft)
      assert(ReferenceTopology.fromTable(table).isLeft)
    }

  property("the exact chain law holds for generated valid fixtures"):
    forAll(TopologyFixtures.validGenerator) { table =>
      val topology = right(TriangleTopology.fromTable(table))
      topology.faces.foreachIndex { face =>
        val coefficients =
          scala.collection.mutable.HashMap.empty[Int, Int].withDefaultValue(0)
        topology.chains.foreachFaceBoundary(face) { (edge, faceCoefficient) =>
          topology.chains.foreachEdgeBoundary(edge) { (vertex, edgeCoefficient) =>
            coefficients.update(
              vertex.ordinal,
              coefficients(vertex.ordinal) +
                faceCoefficient * edgeCoefficient
            )
          }
        }
        assert(coefficients.valuesIterator.forall(_ == 0))
      }
    }

  test("nonorientable fixture has a parity-cycle witness"):
    TriangleTopology.orientAndBuild(TopologyFixtures.nonorientable) match
      case Left(_: OrientationError.NonOrientableCycle) => ()
      case Left(other)                                  => fail(other.message)
      case Right(_) => fail("nonorientable fixture unexpectedly oriented")

  private def right[A](value: Either[TopologyAudit, A]): A =
    value.fold(audit => fail(audit.message), identity)
