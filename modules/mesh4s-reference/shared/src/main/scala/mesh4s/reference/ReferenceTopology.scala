package mesh4s.reference

import mesh4s.Endpoints
import mesh4s.TriangleTable

/** Deliberately slow map-and-set oracle for the trusted topology contract. */
final class ReferenceTopology private (
    val table: TriangleTable,
    val edges: Vector[Endpoints[Int]],
    private val edgeFaces: Map[Endpoints[Int], Vector[Int]],
    private val neighborSets: Vector[Set[Int]],
    private val incidentFaceSets: Vector[Set[Int]]
):
  def vertexCount: Int =
    table.vertexCount

  def edgeCount: Int =
    edges.length

  def faceCount: Int =
    table.faces.length

  def halfedgeCount: Int =
    faceCount * 3

  def boundaryEdgeCount: Int =
    edges.count(edge => edgeFaces(edge).length == 1)

  def neighbors(vertex: Int): Set[Int] =
    neighborSets(vertex)

  def incidentFaces(vertex: Int): Set[Int] =
    incidentFaceSets(vertex)

object ReferenceTopology:
  private final case class Use(face: Int, origin: Int, target: Int)

  def fromTable(
      table: TriangleTable
  ): Either[Vector[String], ReferenceTopology] =
    val localIssues =
      table.faces.zipWithIndex.flatMap { (triangle, face) =>
        val vertices = triangle.toVector
        val bounds =
          vertices.zipWithIndex.collect {
            case (vertex, corner) if vertex < 0 || vertex >= table.vertexCount =>
              s"face $face corner $corner out of bounds: $vertex"
          }
        val repeated =
          if vertices.distinct.length == 3 then Vector.empty
          else Vector(s"face $face repeats a vertex")
        bounds ++ repeated
      }
    val countIssues =
      Option
        .when(table.vertexCount < 0)(
          s"negative vertex count ${table.vertexCount}"
        )
        .toVector
    if countIssues.nonEmpty || localIssues.nonEmpty then Left(countIssues ++ localIssues)
    else
      val duplicateIssues =
        table.faces.zipWithIndex
          .groupBy((triangle, _) => triangle.toVector.sorted)
          .valuesIterator
          .filter(_.length > 1)
          .map(rows =>
            s"duplicate faces ${rows.iterator.map(_._2).toVector.sorted.mkString(",")}"
          )
          .toVector
      if duplicateIssues.nonEmpty then Left(duplicateIssues)
      else buildGlobal(table)

  private def buildGlobal(
      table: TriangleTable
  ): Either[Vector[String], ReferenceTopology] =
    var uses = Map.empty[Endpoints[Int], Vector[Use]]
    var edgeOrder = Vector.empty[Endpoints[Int]]
    table.faces.zipWithIndex.foreach { (triangle, face) =>
      val vertices = triangle.toVector
      var local = 0
      while local < 3 do
        val origin = vertices(local)
        val target = vertices((local + 1) % 3)
        val edge =
          if origin < target then Endpoints(origin, target)
          else Endpoints(target, origin)
        if !uses.contains(edge) then edgeOrder = edgeOrder :+ edge
        uses = uses.updated(
          edge,
          uses.getOrElse(edge, Vector.empty) :+ Use(face, origin, target)
        )
        local += 1
    }

    val edgeIssues =
      edgeOrder.flatMap { edge =>
        val incident = uses(edge)
        if incident.length > 2 then
          Vector(
            s"edge (${edge.first},${edge.second}) has ${incident.length} faces"
          )
        else if incident.length == 2 &&
          incident(0).origin == incident(1).origin
        then
          Vector(
            s"edge (${edge.first},${edge.second}) has conflicting orientation"
          )
        else Vector.empty
      }

    val used =
      table.faces.iterator.flatMap(_.toVector).toSet
    val unusedIssues =
      (0 until table.vertexCount).iterator
        .filterNot(used.contains)
        .map(vertex => s"unused vertex $vertex")
        .toVector
    val linkIssues =
      (0 until table.vertexCount).iterator.flatMap { vertex =>
        val incident =
          table.faces.zipWithIndex.collect {
            case (triangle, face) if triangle.toVector.contains(vertex) =>
              val others = triangle.toVector.filterNot(_ == vertex)
              (others(0), others(1), face)
          }
        if incident.isEmpty then Iterator.empty
        else
          val adjacency =
            incident.foldLeft(Map.empty[Int, Set[Int]]) { case (map, (first, second, _)) =>
              map
                .updated(
                  first,
                  map.getOrElse(first, Set.empty) + second
                )
                .updated(
                  second,
                  map.getOrElse(second, Set.empty) + first
                )
            }
          val componentCount = components(adjacency)
          val degreeOne = adjacency.valuesIterator.count(_.size == 1)
          val degreesValid =
            adjacency.valuesIterator.forall(_.size <= 2) &&
              (degreeOne == 0 || degreeOne == 2)
          Option
            .when(componentCount != 1 || !degreesValid)(
              s"vertex $vertex has a nonmanifold link"
            )
            .iterator
      }.toVector

    val issues = edgeIssues ++ unusedIssues ++ linkIssues
    if issues.nonEmpty then Left(issues)
    else
      val neighborSets =
        Vector.tabulate(table.vertexCount) { vertex =>
          edgeOrder.iterator.collect {
            case Endpoints(`vertex`, second) => second
            case Endpoints(first, `vertex`)  => first
          }.toSet
        }
      val incidentFaceSets =
        Vector.tabulate(table.vertexCount) { vertex =>
          table.faces.zipWithIndex.collect {
            case (triangle, face) if triangle.toVector.contains(vertex) => face
          }.toSet
        }
      Right(
        new ReferenceTopology(
          table,
          edgeOrder,
          uses.view.mapValues(_.map(_.face)).toMap,
          neighborSets,
          incidentFaceSets
        )
      )

  private def components(adjacency: Map[Int, Set[Int]]): Int =
    var unseen = adjacency.keySet
    var count = 0
    while unseen.nonEmpty do
      count += 1
      var frontier = Set(unseen.min)
      unseen = unseen -- frontier
      while frontier.nonEmpty do
        val reached = frontier.iterator.flatMap(adjacency).toSet.intersect(unseen)
        unseen = unseen -- reached
        frontier = reached
    count
