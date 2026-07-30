package mesh4s

import locus4s.data.Field
import locus4s.data.VectorField

import scala.collection.mutable

enum OrientationError derives CanEqual:
  case InvalidInput(audit: TopologyAudit)
  case NonOrientableCycle(
      faceCycle: Vector[Int],
      closingEdge: Endpoints[Int]
  )
  case OrientedTopologyInvalid(audit: TopologyAudit)

  def message: String =
    this match
      case InvalidInput(audit) =>
        audit.message
      case NonOrientableCycle(faceCycle, closingEdge) =>
        s"face-orientation parity is contradictory around cycle ${faceCycle.mkString(" -> ")} closing across edge (${closingEdge.first}, ${closingEdge.second})"
      case OrientedTopologyInvalid(audit) =>
        s"oriented rows still violate topology laws:\n${audit.message}"

final class OrientedBuild private[mesh4s] (
    val topology: TriangleTopology
)(
    val flipped: Field[topology.Face, Boolean]
)

private[mesh4s] object TopologyOrienter:
  private final case class EdgeKey(first: Int, second: Int)

  private object EdgeKey:
    def canonical(first: Int, second: Int): EdgeKey =
      if first < second then EdgeKey(first, second)
      else EdgeKey(second, first)

  private final case class Use(face: Int, origin: Int)
  private final case class Constraint(
      neighbor: Int,
      flipParity: Boolean,
      edge: EdgeKey
  )

  def orientAndBuild(
      table: TriangleTable,
      issueLimit: Int
  ): Either[OrientationError, OrientedBuild] =
    admissibilityAudit(table, issueLimit) match
      case Some(audit) =>
        Left(OrientationError.InvalidInput(audit))
      case None =>
        orient(table) match
          case Left(error)  => Left(error)
          case Right(flips) =>
            val rows =
              table.faces.zip(flips).map {
                case (triangle, false) => triangle
                case (triangle, true)  =>
                  Triangle(triangle.first, triangle.third, triangle.second)
              }
            TriangleTopology
              .fromOrdinalFaces(table.vertexCount, rows, issueLimit)
              .left
              .map(OrientationError.OrientedTopologyInvalid.apply)
              .map { topology =>
                val field =
                  VectorField
                    .fromValues(topology.faces, flips)
                    .fold(
                      error => throw new IllegalStateException(error.message),
                      identity
                    )
                new OrientedBuild(topology)(field)
              }

  private def admissibilityAudit(
      table: TriangleTable,
      issueLimit: Int
  ): Option[TopologyAudit] =
    TopologyBuilder
      .audit(table, issueLimit, checkOrientation = false)
      .flatMap { audit =>
        val admissibilityIssues = audit.issues.filter {
          case _: TopologyIssue.NegativeVertexCount    => true
          case _: TopologyIssue.VertexOutOfBounds      => true
          case _: TopologyIssue.RepeatedVertex         => true
          case _: TopologyIssue.DuplicateFace          => true
          case _: TopologyIssue.NonManifoldEdge        => true
          case _: TopologyIssue.AddressabilityExceeded => true
          case _                                       => false
        }
        Option.when(admissibilityIssues.nonEmpty)(
          TopologyAudit(
            admissibilityIssues,
            admissibilityIssues.length.toLong,
            audit.truncated,
            audit.completedStages
          )
        )
      }

  private def orient(
      table: TriangleTable
  ): Either[OrientationError, Vector[Boolean]] =
    val uses =
      mutable.HashMap.empty[EdgeKey, mutable.ArrayBuffer[Use]]
    table.faces.zipWithIndex.foreach { (triangle, face) =>
      val vertices = triangle.toVector
      var local = 0
      while local < 3 do
        val origin = vertices(local)
        val target = vertices((local + 1) % 3)
        uses
          .getOrElseUpdate(
            EdgeKey.canonical(origin, target),
            mutable.ArrayBuffer.empty
          )
          .addOne(Use(face, origin))
        local += 1
    }

    val adjacency =
      Array.fill(table.faces.length)(mutable.ArrayBuffer.empty[Constraint])
    uses.foreach { (edge, edgeUses) =>
      if edgeUses.length == 2 then
        val first = edgeUses(0)
        val second = edgeUses(1)
        val parity = first.origin == second.origin
        adjacency(first.face).addOne(Constraint(second.face, parity, edge))
        adjacency(second.face).addOne(Constraint(first.face, parity, edge)): Unit
    }
    adjacency.foreach(_.sortInPlaceBy(_.neighbor))

    val flip = Array.fill[Option[Boolean]](table.faces.length)(None)
    val parent = Array.fill(table.faces.length)(-1)
    var contradiction = Option.empty[OrientationError]
    var seed = 0
    while seed < table.faces.length && contradiction.isEmpty do
      if flip(seed).isEmpty then
        flip(seed) = Some(false)
        val queue = mutable.ArrayDeque(seed)
        while queue.nonEmpty && contradiction.isEmpty do
          val face = queue.removeHead()
          val constraints = adjacency(face)
          var index = 0
          while index < constraints.length && contradiction.isEmpty do
            val constraint = constraints(index)
            val expected = flip(face).get ^ constraint.flipParity
            flip(constraint.neighbor) match
              case None =>
                flip(constraint.neighbor) = Some(expected)
                parent(constraint.neighbor) = face
                queue.append(constraint.neighbor)
              case Some(actual) if actual != expected =>
                contradiction = Some(
                  OrientationError.NonOrientableCycle(
                    cycleWitness(face, constraint.neighbor, parent),
                    Endpoints(
                      constraint.edge.first,
                      constraint.edge.second
                    )
                  )
                )
              case Some(_) => ()
            index += 1
      seed += 1
    contradiction match
      case Some(error) => Left(error)
      case None        => Right(flip.iterator.map(_.getOrElse(false)).toVector)

  private def cycleWitness(
      first: Int,
      second: Int,
      parent: Array[Int]
  ): Vector[Int] =
    val firstPath = pathToRoot(first, parent)
    val secondPath = pathToRoot(second, parent)
    val secondPositions = secondPath.zipWithIndex.toMap
    val (common, firstIndex) =
      firstPath.zipWithIndex.find((face, _) => secondPositions.contains(face)).get
    val secondIndex = secondPositions(common)
    firstPath.take(firstIndex + 1) ++
      secondPath.take(secondIndex).reverse

  private def pathToRoot(face: Int, parent: Array[Int]): Vector[Int] =
    val output = Vector.newBuilder[Int]
    var current = face
    while current >= 0 do
      output += current
      current = parent(current)
    output.result()
