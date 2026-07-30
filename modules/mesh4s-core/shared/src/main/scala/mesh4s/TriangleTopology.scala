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
  def foreachFaceHalfedge(face: FaceIndex)(f: HalfedgeIndex => Unit): Unit
  def foreachBoundaryHalfedge(f: HalfedgeIndex => Unit): Unit

  def fan(vertex: VertexIndex): VertexFan[
    HalfedgeIndex,
    FaceIndex,
    VertexIndex
  ]

  def boundaryLoops: Vector[Cycle[HalfedgeIndex]]
  def vertexComponents: Vector[Region[Vertex]]

  def boundaryHalfedgeCount: Int
  def isClosed: Boolean =
    boundaryHalfedgeCount == 0

  def componentCount: Int =
    vertexComponents.length

  def isConnected: Boolean =
    componentCount <= 1

  def eulerCharacteristic: Int =
    vertices.size - edges.size + faces.size

  def summary: TopologySummary =
    TopologySummary(
      vertices.size,
      edges.size,
      faces.size,
      halfedges.size,
      boundaryLoops.length,
      componentCount,
      eulerCharacteristic
    )

  def chains: SurfaceChains[this.type] =
    new SurfaceChains(this)

  def requireClosed: Either[BoundaryReport, ClosedTopology[this.type]] =
    val report =
      BoundaryReport(
        boundaryLoops.map(_.values.map(_.ordinal))
      )
    if report.loopHalfedgeOrdinals.isEmpty then Right(new ClosedTopology(this, report))
    else Left(report)

  def requireConnected: Either[
    ComponentReport,
    ConnectedTopology[this.type]
  ] =
    val report =
      ComponentReport(
        vertexComponents.map(_.ordinalsInDomainOrder.toVector)
      )
    if report.componentVertexOrdinals.length <= 1 then
      Right(new ConnectedTopology(this, report))
    else Left(report)

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

  def on[V](vertices: FiniteDomain[V]): TopologyOn[V] =
    new TopologyOn(vertices)

  def inspect(
      table: TriangleTable,
      issueLimit: Int = TopologyAudit.DefaultIssueLimit
  ): Option[TopologyAudit] =
    TopologyBuilder.audit(table, issueLimit, checkOrientation = true)

  def orientAndBuild(
      table: TriangleTable,
      issueLimit: Int = TopologyAudit.DefaultIssueLimit
  ): Either[OrientationError, OrientedBuild] =
    TopologyOrienter.orientAndBuild(table, issueLimit)

final class TopologyOn[V] private[mesh4s] (
    val vertices: FiniteDomain[V]
):
  def fromOrientedFaces(
      faces: IterableOnce[Triangle[Index[V]]],
      issueLimit: Int = TopologyAudit.DefaultIssueLimit
  ): Either[TopologyAudit, TriangleTopology { type Vertex = V }] =
    TopologyBuilder.buildOn(vertices, faces, issueLimit)

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

  def foreachFaceHalfedge(face: FaceIndex)(f: HalfedgeIndex => Unit): Unit =
    val start = face.ordinal * 3
    f(checkedIndex(halfedges, start))
    f(checkedIndex(halfedges, start + 1))
    f(checkedIndex(halfedges, start + 2))

  def foreachBoundaryHalfedge(f: HalfedgeIndex => Unit): Unit =
    var ordinal = 0
    while ordinal < opposites.length do
      if opposites(ordinal) == TriangleTopology.BoundarySentinel then
        f(checkedIndex(halfedges, ordinal))
      ordinal += 1

  def fan(
      vertex: VertexIndex
  ): VertexFan[HalfedgeIndex, FaceIndex, VertexIndex] =
    val outgoing = incidentFaceRows(vertex.ordinal).iterator.map { face =>
      val start = face * 3
      val corner =
        if faceVertices(start) == vertex.ordinal then start
        else if faceVertices(start + 1) == vertex.ordinal then start + 1
        else start + 2
      val after = faceVertices(nextOrdinal(corner))
      val before = faceVertices(previousOrdinal(corner))
      (after, before, corner)
    }.toVector
    val nextByNeighbor =
      outgoing.iterator.map { (after, before, corner) =>
        after -> (before, corner)
      }.toMap
    val incoming = outgoing.iterator.map(_._2).toSet
    val boundaryStarts =
      nextByNeighbor.keySet.diff(incoming)
    val isBoundary = boundaryStarts.nonEmpty
    val start =
      if isBoundary then boundaryStarts.min
      else nextByNeighbor.keysIterator.min

    val orderedNeighbors = Vector.newBuilder[VertexIndex]
    val orderedHalfedges = Vector.newBuilder[HalfedgeIndex]
    val orderedFaces = Vector.newBuilder[FaceIndex]
    var current = start
    var visited = 0
    while visited < outgoing.length do
      orderedNeighbors += checkedIndex(vertices, current)
      val (following, corner) = nextByNeighbor(current)
      orderedHalfedges += checkedIndex(halfedges, corner)
      orderedFaces += checkedIndex(faces, corner / 3)
      current = following
      visited += 1
    if isBoundary then
      orderedNeighbors += checkedIndex(vertices, current)
      VertexFan.Boundary(
        Path.unsafe(orderedHalfedges.result()),
        Path.unsafe(orderedFaces.result()),
        Path.unsafe(orderedNeighbors.result())
      )
    else
      VertexFan.Interior(
        Cycle.unsafe(orderedHalfedges.result()),
        Cycle.unsafe(orderedFaces.result()),
        Cycle.unsafe(orderedNeighbors.result())
      )

  def boundaryLoops: Vector[Cycle[HalfedgeIndex]] =
    val unseen =
      scala.collection.mutable.TreeSet.from(
        opposites.indices.filter(ordinal =>
          opposites(ordinal) == TriangleTopology.BoundarySentinel
        )
      )
    val output = Vector.newBuilder[Cycle[HalfedgeIndex]]
    while unseen.nonEmpty do
      val start = unseen.min
      val loop = Vector.newBuilder[HalfedgeIndex]
      loop += checkedIndex(halfedges, start)
      unseen.remove(start)
      var current = nextBoundaryOrdinal(start)
      while current != start do
        loop += checkedIndex(halfedges, current)
        unseen.remove(current)
        current = nextBoundaryOrdinal(current)
      output += Cycle.unsafe(loop.result())
    output.result()

  def vertexComponents: Vector[Region[Vertex]] =
    val unseen = scala.collection.mutable.TreeSet.from(vertices.indices.map(_.ordinal))
    val output = Vector.newBuilder[Region[Vertex]]
    while unseen.nonEmpty do
      val start = unseen.min
      val queue = scala.collection.mutable.ArrayDeque(start)
      val component = Vector.newBuilder[Int]
      unseen.remove(start)
      while queue.nonEmpty do
        val current = queue.removeHead()
        component += current
        val row = neighborRows(current)
        var index = 0
        while index < row.length do
          val neighbor = row(index)
          if unseen.remove(neighbor) then queue.append(neighbor)
          index += 1
      output +=
        Region
          .fromOrdinals(vertices, component.result())
          .fold(
            error =>
              throw new IllegalStateException(
                s"validated component contained ${error.message}"
              ),
            identity
          )
    output.result()

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

  private def nextOrdinal(halfedge: Int): Int =
    val start = halfedge - halfedge % 3
    start + (halfedge + 1) % 3

  private def previousOrdinal(halfedge: Int): Int =
    val start = halfedge - halfedge % 3
    start + (halfedge + 2) % 3

  private def nextBoundaryOrdinal(halfedge: Int): Int =
    var candidate = nextOrdinal(halfedge)
    while opposites(candidate) != TriangleTopology.BoundarySentinel do
      candidate = nextOrdinal(opposites(candidate))
    candidate
