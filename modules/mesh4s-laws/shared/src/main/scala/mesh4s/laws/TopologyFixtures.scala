package mesh4s.laws

import mesh4s.Triangle
import mesh4s.TriangleTable
import org.scalacheck.Gen

object TopologyFixtures:
  val disk: TriangleTable =
    TriangleTable(
      4,
      Vector(
        Triangle(0, 1, 2),
        Triangle(0, 2, 3)
      )
    )

  val annulus: TriangleTable =
    TriangleTable(
      8,
      Vector
        .tabulate(4) { index =>
          val next = (index + 1) % 4
          Vector(
            Triangle(4 + index, 4 + next, next),
            Triangle(4 + index, next, index)
          )
        }
        .flatten
    )

  val sphere: TriangleTable =
    TriangleTable(
      4,
      Vector(
        Triangle(0, 2, 1),
        Triangle(0, 1, 3),
        Triangle(1, 2, 3),
        Triangle(2, 0, 3)
      )
    )

  val torus: TriangleTable =
    periodicTorus(4, 4)

  val disconnected: TriangleTable =
    disjointUnion(disk, sphere)

  val boundaryPatch: TriangleTable =
    TriangleTable(3, Vector(Triangle(0, 1, 2)))

  val valid: Vector[TriangleTable] =
    Vector(disk, annulus, sphere, torus, disconnected, boundaryPatch)

  val outOfRange: TriangleTable =
    TriangleTable(3, Vector(Triangle(0, 1, 3)))

  val repeatedCorner: TriangleTable =
    TriangleTable(3, Vector(Triangle(0, 1, 1)))

  val duplicateFace: TriangleTable =
    TriangleTable(
      3,
      Vector(Triangle(0, 1, 2), Triangle(2, 1, 0))
    )

  val threeFaceEdge: TriangleTable =
    TriangleTable(
      5,
      Vector(
        Triangle(0, 1, 2),
        Triangle(1, 0, 3),
        Triangle(0, 1, 4)
      )
    )

  val orientationConflict: TriangleTable =
    TriangleTable(
      4,
      Vector(Triangle(0, 1, 2), Triangle(0, 1, 3))
    )

  val bowTie: TriangleTable =
    TriangleTable(
      5,
      Vector(Triangle(0, 1, 2), Triangle(0, 3, 4))
    )

  val unusedVertex: TriangleTable =
    TriangleTable(4, Vector(Triangle(0, 1, 2)))

  val nonorientable: TriangleTable =
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

  val malformed: Vector[TriangleTable] =
    Vector(
      outOfRange,
      repeatedCorner,
      duplicateFace,
      threeFaceEdge,
      orientationConflict,
      bowTie,
      unusedVertex,
      nonorientable
    )

  val validGenerator: Gen[TriangleTable] =
    Gen.oneOf(valid)

  val malformedGenerator: Gen[TriangleTable] =
    Gen.oneOf(malformed)

  private def periodicTorus(rows: Int, columns: Int): TriangleTable =
    def vertex(row: Int, column: Int): Int =
      (row % rows) * columns + column % columns

    val faces =
      Vector
        .tabulate(rows, columns) { (row, column) =>
          val v00 = vertex(row, column)
          val v10 = vertex(row + 1, column)
          val v11 = vertex(row + 1, column + 1)
          val v01 = vertex(row, column + 1)
          Vector(
            Triangle(v00, v10, v11),
            Triangle(v00, v11, v01)
          )
        }
        .flatten
        .flatten
    TriangleTable(rows * columns, faces)

  private def disjointUnion(
      first: TriangleTable,
      second: TriangleTable
  ): TriangleTable =
    val offset = first.vertexCount
    TriangleTable(
      first.vertexCount + second.vertexCount,
      first.faces ++ second.faces.map(
        _.map(_ + offset)
      )
    )
