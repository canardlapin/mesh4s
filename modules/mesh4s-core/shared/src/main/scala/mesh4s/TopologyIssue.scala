package mesh4s

enum CellKind derives CanEqual:
  case Vertex, Edge, Face, Halfedge

enum AuditStage derives CanEqual:
  case Addressability
  case LocalFaces
  case DuplicateFaces
  case EdgeIncidence
  case Orientation
  case VertexUsage
  case VertexLinks

enum TopologyIssue derives CanEqual:
  case NegativeVertexCount(requested: Int)
  case VertexOutOfBounds(face: Int, corner: Int, vertexOrdinal: Int)
  case RepeatedVertex(face: Int, vertex: Int)
  case DuplicateFace(first: Int, duplicate: Int)
  case NonManifoldEdge(
      firstVertex: Int,
      secondVertex: Int,
      incidentFaces: Vector[Int]
  )
  case OrientationConflict(
      firstVertex: Int,
      secondVertex: Int,
      leftFace: Int,
      rightFace: Int
  )
  case NonManifoldVertex(
      vertex: Int,
      fanCount: Int,
      witnessFaces: Vector[Int]
  )
  case UnusedVertex(vertex: Int)
  case AddressabilityExceeded(kind: CellKind, requested: Long)

  def message: String =
    this match
      case NegativeVertexCount(requested) =>
        s"vertex count must be nonnegative, found $requested"
      case VertexOutOfBounds(face, corner, ordinal) =>
        s"face $face corner $corner references vertex $ordinal outside the vertex domain"
      case RepeatedVertex(face, vertex) =>
        s"face $face repeats vertex $vertex"
      case DuplicateFace(first, duplicate) =>
        s"face $duplicate duplicates face $first"
      case NonManifoldEdge(first, second, incidentFaces) =>
        s"edge ($first, $second) has ${incidentFaces.length} incident faces: ${incidentFaces.mkString(", ")}"
      case OrientationConflict(first, second, left, right) =>
        s"faces $left and $right traverse shared edge ($first, $second) in the same direction"
      case NonManifoldVertex(vertex, fanCount, witnessFaces) =>
        s"vertex $vertex has $fanCount disjoint or branching fans; witness faces: ${witnessFaces.mkString(", ")}"
      case UnusedVertex(vertex) =>
        s"vertex $vertex is not incident to any face"
      case AddressabilityExceeded(kind, requested) =>
        s"$kind storage requires $requested entries, exceeding Int addressability"

final case class TopologyAudit(
    issues: Vector[TopologyIssue],
    totalIssueCount: Long,
    truncated: Boolean,
    completedStages: Vector[AuditStage]
):
  require(issues.nonEmpty, "a failed topology audit must contain an issue")

  def message: String =
    val suffix =
      if truncated then s"\n... ${totalIssueCount - issues.length} additional issues"
      else ""
    issues.map(_.message).mkString("\n") + suffix

object TopologyAudit:
  val DefaultIssueLimit: Int = 1024
