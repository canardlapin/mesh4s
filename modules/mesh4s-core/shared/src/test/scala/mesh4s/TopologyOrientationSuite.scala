package mesh4s

class TopologyOrientationSuite extends munit.FunSuite:
  test("orientAndBuild preserves row order and reports exact flips"):
    val result =
      right(
        TriangleTopology.orientAndBuild(
          TriangleTable(
            4,
            Vector(
              Triangle(0, 1, 2),
              Triangle(0, 1, 3)
            )
          )
        )
      )

    assertEquals(result.flipped.toVector, Vector(false, true))
    assertEquals(
      result.topology.verticesOf(index(result.topology.faces, 0)).map(_.ordinal),
      Triangle(0, 1, 2)
    )
    assertEquals(
      result.topology.verticesOf(index(result.topology.faces, 1)).map(_.ordinal),
      Triangle(0, 3, 1)
    )

  test("the first face of each edge-connected component is preserved"):
    val result =
      right(
        TriangleTopology.orientAndBuild(
          TriangleTable(
            8,
            Vector(
              Triangle(0, 1, 2),
              Triangle(0, 1, 3),
              Triangle(4, 5, 6),
              Triangle(4, 5, 7)
            )
          )
        )
      )
    assertEquals(
      result.flipped.toVector,
      Vector(false, true, false, true)
    )

  test("orientation rejects edge-nonmanifold input before propagation"):
    val error =
      left(
        TriangleTopology.orientAndBuild(
          TriangleTable(
            5,
            Vector(
              Triangle(0, 1, 2),
              Triangle(1, 0, 3),
              Triangle(0, 1, 4)
            )
          )
        )
      )
    error match
      case OrientationError.InvalidInput(audit) =>
        assert(audit.issues.exists {
          case _: TopologyIssue.NonManifoldEdge => true
          case _                                => false
        })
      case other => fail(s"expected invalid input, found ${other.message}")

  test("a nonorientable parity cycle returns face-cycle evidence"):
    val mobius =
      TriangleTable(
        6,
        Vector(
          Triangle(0, 1, 2),
          Triangle(1, 3, 2),
          Triangle(2, 3, 4),
          Triangle(3, 5, 4),
          Triangle(4, 5, 1),
          Triangle(5, 0, 1)
        )
      )
    left(TriangleTopology.orientAndBuild(mobius)) match
      case OrientationError.NonOrientableCycle(cycle, closingEdge) =>
        assert(cycle.length >= 3)
        assert(closingEdge.first < closingEdge.second)
      case other =>
        fail(s"expected nonorientable cycle, found ${other.message}")

  test("post-orientation strict validation still rejects bow ties"):
    val error =
      left(
        TriangleTopology.orientAndBuild(
          TriangleTable(
            5,
            Vector(
              Triangle(0, 1, 2),
              Triangle(0, 3, 4)
            )
          )
        )
      )
    error match
      case OrientationError.OrientedTopologyInvalid(audit) =>
        assert(audit.issues.exists {
          case _: TopologyIssue.NonManifoldVertex => true
          case _                                  => false
        })
      case other =>
        fail(s"expected post-orientation topology issue, found ${other.message}")

  private def index[S](
      domain: locus4s.FiniteDomain[S],
      ordinal: Int
  ): locus4s.Index[S] =
    domain.index(ordinal).fold(error => fail(error.message), identity)

  private def right(value: Either[OrientationError, OrientedBuild]): OrientedBuild =
    value.fold(error => fail(error.message), identity)

  private def left(value: Either[OrientationError, OrientedBuild]): OrientationError =
    value.fold(identity, _ => fail("expected orientation to fail"))
