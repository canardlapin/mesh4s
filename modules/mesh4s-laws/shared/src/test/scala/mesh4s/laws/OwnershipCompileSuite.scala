package mesh4s.laws

import scala.compiletime.testing.typeCheckErrors

final class OwnershipCompileSuite extends munit.FunSuite:
  test("a face cannot be passed to a vertex operation"):
    val errors =
      typeCheckErrors(
        """
          import mesh4s.*
          val topology =
            TriangleTopology
              .fromOrdinalFaces(3, Vector(Triangle(0, 1, 2)))
              .toOption
              .get
          val face = topology.faces.index(0).toOption.get
          topology.neighbors(face)
        """
      )
    assert(errors.nonEmpty)

  test("a vertex from another topology requires explicit transport"):
    val errors =
      typeCheckErrors(
        """
          import mesh4s.*
          val left =
            TriangleTopology
              .fromOrdinalFaces(3, Vector(Triangle(0, 1, 2)))
              .toOption
              .get
          val right =
            TriangleTopology
              .fromOrdinalFaces(3, Vector(Triangle(0, 1, 2)))
              .toOption
              .get
          val leftVertex = left.vertices.index(0).toOption.get
          right.neighbors(leftVertex)
        """
      )
    assert(errors.nonEmpty)

  test("an index from another finite domain cannot become a mesh vertex"):
    val errors =
      typeCheckErrors(
        """
          import locus4s.FiniteDomain
          import mesh4s.*
          val topology =
            TriangleTopology
              .fromOrdinalFaces(3, Vector(Triangle(0, 1, 2)))
              .toOption
              .get
          val voxelDomain =
            FiniteDomain.ephemeral("voxel", 3).toOption.get.value
          val voxel = voxelDomain.index(0).toOption.get
          topology.neighbors(voxel)
        """
      )
    assert(errors.nonEmpty)

  test("the positive owner-safe path remains concise"):
    val errors =
      typeCheckErrors(
        """
          import mesh4s.*
          val topology =
            TriangleTopology
              .fromOrdinalFaces(3, Vector(Triangle(0, 1, 2)))
              .toOption
              .get
          val vertex = topology.vertices.index(0).toOption.get
          topology.neighbors(vertex)
        """
      )
    assert(errors.isEmpty)
