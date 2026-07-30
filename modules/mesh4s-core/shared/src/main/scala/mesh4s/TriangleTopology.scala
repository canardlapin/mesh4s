package mesh4s

import locus4s.FiniteDomain
import locus4s.Index
import locus4s.Region

/** Immutable owner of one finite oriented triangular 2-manifold. */
sealed abstract class TriangleTopology:
  type Vertex
  type Edge
  type Face
  type Halfedge

  val vertices: FiniteDomain[Vertex]
  val edges: FiniteDomain[Edge]
  val faces: FiniteDomain[Face]
  val halfedges: FiniteDomain[Halfedge]

  type VertexIndex = Index[Vertex]
  type EdgeIndex = Index[Edge]
  type FaceIndex = Index[Face]
  type HalfedgeIndex = Index[Halfedge]

  def origin(halfedge: HalfedgeIndex): VertexIndex
  def target(halfedge: HalfedgeIndex): VertexIndex
  def next(halfedge: HalfedgeIndex): HalfedgeIndex
  def previous(halfedge: HalfedgeIndex): HalfedgeIndex
  def opposite(halfedge: HalfedgeIndex): Option[HalfedgeIndex]
  def faceOf(halfedge: HalfedgeIndex): FaceIndex
  def edgeOf(halfedge: HalfedgeIndex): EdgeIndex
  def verticesOf(face: FaceIndex): Triangle[VertexIndex]
  def endpointsOf(edge: EdgeIndex): Endpoints[VertexIndex]

  def neighbors(vertex: VertexIndex): Region[Vertex]
  def incidentFaces(vertex: VertexIndex): Region[Face]

  def foreachNeighbor(vertex: VertexIndex)(f: VertexIndex => Unit): Unit
  def foreachIncidentFace(vertex: VertexIndex)(f: FaceIndex => Unit): Unit

  def boundaryHalfedgeCount: Int
  def isClosed: Boolean =
    boundaryHalfedgeCount == 0

  private[mesh4s] def faceVertexOrdinals: Array[Int]
  private[mesh4s] def oppositeOrdinals: Array[Int]
  private[mesh4s] def edgeOfHalfedgeOrdinals: Array[Int]
  private[mesh4s] def halfedgeOfEdgeOrdinals: Array[Int]

object TriangleTopology:
  private[mesh4s] val BoundarySentinel: Int = -1

  def fromOrdinalFaces(
      vertexCount: Int,
      faces: IterableOnce[Triangle[Int]],
      issueLimit: Int = TopologyAudit.DefaultIssueLimit
  ): Either[TopologyAudit, TriangleTopology] =
    fromTable(TriangleTable(vertexCount, faces.iterator.toVector), issueLimit)

  def fromTable(
      table: TriangleTable,
      issueLimit: Int = TopologyAudit.DefaultIssueLimit
  ): Either[TopologyAudit, TriangleTopology] =
    TopologyBuilder.build(table, issueLimit)

  def inspect(
      table: TriangleTable,
      issueLimit: Int = TopologyAudit.DefaultIssueLimit
  ): Option[TopologyAudit] =
    TopologyBuilder.audit(table, issueLimit, checkOrientation = true)

private final class PackedTopology[V, E, F, H](
    val vertices: FiniteDomain[V],
    val edges: FiniteDomain[E],
    val faces: FiniteDomain[F],
    val halfedges: FiniteDomain[H],
    private val faceVertices: Array[Int],
    private val opposites: Array[Int],
    private val edgeOfHalfedge: Array[Int],
    private val halfedgeOfEdge: Array[Int],
    private val incidentFaceRows: Array[Array[Int]],
    private val neighborRows: Array[Array[Int]]
) extends TriangleTopology:
  type Vertex = V
  type Edge = E
  type Face = F
  type Halfedge = H

  def origin(halfedge: HalfedgeIndex): VertexIndex =
    checkedIndex(vertices, faceVertices(halfedge.ordinal))

  def target(halfedge: HalfedgeIndex): VertexIndex =
    origin(next(halfedge))

  def next(halfedge: HalfedgeIndex): HalfedgeIndex =
    val ordinal = halfedge.ordinal
    val faceStart = ordinal - ordinal % 3
    checkedIndex(halfedges, faceStart + (ordinal + 1) % 3)

  def previous(halfedge: HalfedgeIndex): HalfedgeIndex =
    val ordinal = halfedge.ordinal
    val faceStart = ordinal - ordinal % 3
    checkedIndex(halfedges, faceStart + (ordinal + 2) % 3)

  def opposite(halfedge: HalfedgeIndex): Option[HalfedgeIndex] =
    val ordinal = opposites(halfedge.ordinal)
    if ordinal == TriangleTopology.BoundarySentinel then None
    else halfedges.indexOption(ordinal)

  def faceOf(halfedge: HalfedgeIndex): FaceIndex =
    checkedIndex(faces, halfedge.ordinal / 3)

  def edgeOf(halfedge: HalfedgeIndex): EdgeIndex =
    checkedIndex(edges, edgeOfHalfedge(halfedge.ordinal))

  def verticesOf(face: FaceIndex): Triangle[VertexIndex] =
    val start = face.ordinal * 3
    Triangle(
      checkedIndex(vertices, faceVertices(start)),
      checkedIndex(vertices, faceVertices(start + 1)),
      checkedIndex(vertices, faceVertices(start + 2))
    )

  def endpointsOf(edge: EdgeIndex): Endpoints[VertexIndex] =
    val halfedge = checkedIndex(halfedges, halfedgeOfEdge(edge.ordinal))
    val first = origin(halfedge)
    val second = target(halfedge)
    if first.ordinal < second.ordinal then Endpoints(first, second)
    else Endpoints(second, first)

  def neighbors(vertex: VertexIndex): Region[Vertex] =
    Region.fromIndices(
      vertices,
      neighborRows(vertex.ordinal).iterator.map(checkedIndex(vertices, _))
    )

  def incidentFaces(vertex: VertexIndex): Region[Face] =
    Region.fromIndices(
      faces,
      incidentFaceRows(vertex.ordinal).iterator.map(checkedIndex(faces, _))
    )

  def foreachNeighbor(vertex: VertexIndex)(f: VertexIndex => Unit): Unit =
    val row = neighborRows(vertex.ordinal)
    var index = 0
    while index < row.length do
      f(checkedIndex(vertices, row(index)))
      index += 1

  def foreachIncidentFace(vertex: VertexIndex)(f: FaceIndex => Unit): Unit =
    val row = incidentFaceRows(vertex.ordinal)
    var index = 0
    while index < row.length do
      f(checkedIndex(faces, row(index)))
      index += 1

  val boundaryHalfedgeCount: Int =
    opposites.count(_ == TriangleTopology.BoundarySentinel)

  private[mesh4s] def faceVertexOrdinals: Array[Int] =
    faceVertices.clone()

  private[mesh4s] def oppositeOrdinals: Array[Int] =
    opposites.clone()

  private[mesh4s] def edgeOfHalfedgeOrdinals: Array[Int] =
    edgeOfHalfedge.clone()

  private[mesh4s] def halfedgeOfEdgeOrdinals: Array[Int] =
    halfedgeOfEdge.clone()

  private def checkedIndex[S](
      domain: FiniteDomain[S],
      ordinal: Int
  ): Index[S] =
    domain
      .index(ordinal)
      .fold(
        error =>
          throw new IllegalStateException(
            s"validated topology contained ${error.message}"
          ),
        identity
      )
