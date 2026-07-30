package mesh4s

import locus4s.FiniteDomain
import locus4s.Index
import locus4s.SomeFiniteDomain

import scala.collection.mutable

private[mesh4s] object TopologyBuilder:
  private final case class EdgeKey(first: Int, second: Int)

  private object EdgeKey:
    def canonical(first: Int, second: Int): EdgeKey =
      if first < second then EdgeKey(first, second)
      else EdgeKey(second, first)

  private final case class EdgeUse(
      face: Int,
      origin: Int,
      target: Int
  )

  private final case class CompiledStorage(
      edgeCount: Int,
      faceVertices: Array[Int],
      opposites: Array[Int],
      edgeOfHalfedge: Array[Int],
      halfedgeOfEdge: Array[Int],
      incidentFaces: Array[Array[Int]],
      neighbors: Array[Array[Int]]
  )

  private final class AuditCollector(limit: Int):
    private val stored = Vector.newBuilder[TopologyIssue]
    private val completed = mutable.ArrayBuffer.empty[AuditStage]
    private var total = 0L

    def add(issue: TopologyIssue): Unit =
      if total < limit.toLong then stored += issue
      total += 1L

    def complete(stage: AuditStage): Unit =
      completed.addOne(stage)

    def result(): Option[TopologyAudit] =
      if total == 0L then None
      else
        Some(
          TopologyAudit(
            stored.result(),
            total,
            total > limit.toLong,
            completed.toVector
          )
        )

  def build(
      table: TriangleTable,
      issueLimit: Int
  ): Either[TopologyAudit, TriangleTopology] =
    audit(table, issueLimit, checkOrientation = true) match
      case Some(report) => Left(report)
      case None         => Right(compile(table))

  def buildOn[V](
      vertices: FiniteDomain[V],
      faces: IterableOnce[Triangle[Index[V]]],
      issueLimit: Int
  ): Either[TopologyAudit, TriangleTopology { type Vertex = V }] =
    val table =
      TriangleTable(
        vertices.size,
        faces.iterator.map(_.map(_.ordinal)).toVector
      )
    audit(table, issueLimit, checkOrientation = true) match
      case Some(report) => Left(report)
      case None         => Right(compileOn(table, vertices))

  def buildPersistent[V, E, F, H](
      record: TriangleTopologyRecord,
      vertices: FiniteDomain[V],
      edges: FiniteDomain[E],
      faces: FiniteDomain[F],
      halfedges: FiniteDomain[H]
  ): Either[
    TopologyAudit,
    TriangleTopology {
      type Vertex = V
      type Edge = E
      type Face = F
      type Halfedge = H
    }
  ] =
    val table = record.table
    audit(
      table,
      TopologyAudit.DefaultIssueLimit,
      checkOrientation = true
    ) match
      case Some(report) =>
        Left(report)
      case None =>
        val storage = compileStorage(table)
        if vertices.size != table.vertexCount ||
          edges.size != storage.edgeCount ||
          faces.size != table.faces.length ||
          halfedges.size != storage.faceVertices.length
        then
          throw new IllegalStateException(
            "validated topology record derived inconsistent cell-domain sizes"
          )
        else
          Right(
            packPersistent(
              vertices,
              edges,
              faces,
              halfedges,
              storage,
              record
            )
          )

  def audit(
      table: TriangleTable,
      issueLimit: Int,
      checkOrientation: Boolean
  ): Option[TopologyAudit] =
    val collector = new AuditCollector(math.max(1, issueLimit))
    val vertexCount = table.vertexCount
    val faceCount = table.faces.length
    if vertexCount < 0 then collector.add(TopologyIssue.NegativeVertexCount(vertexCount))

    val halfedgeCount = faceCount.toLong * 3L
    if halfedgeCount > Int.MaxValue.toLong then
      collector.add(
        TopologyIssue.AddressabilityExceeded(
          CellKind.Halfedge,
          halfedgeCount
        )
      )
    collector.complete(AuditStage.Addressability)

    val validFace = Array.fill(faceCount)(true)
    var allLocalFacesValid = true
    var duplicateFound = false
    val usedVertices =
      if vertexCount >= 0 then Array.fill(vertexCount)(false)
      else Array.emptyBooleanArray
    val firstFaceForTriangle = mutable.HashMap.empty[(Int, Int, Int), Int]
    val edgeUses =
      mutable.LinkedHashMap.empty[EdgeKey, mutable.ArrayBuffer[EdgeUse]]

    table.faces.zipWithIndex.foreach { (triangle, face) =>
      val values = triangle.toVector
      values.zipWithIndex.foreach { (vertex, corner) =>
        if vertex < 0 || vertex >= vertexCount then
          collector.add(
            TopologyIssue.VertexOutOfBounds(face, corner, vertex)
          )
          validFace(face) = false
          allLocalFacesValid = false
        else usedVertices(vertex) = true
      }

      val repeated = values.groupBy(identity).collect {
        case (vertex, occurrences) if occurrences.length > 1 => vertex
      }
      repeated.toVector.sorted.foreach(vertex =>
        collector.add(TopologyIssue.RepeatedVertex(face, vertex))
      )
      if repeated.nonEmpty then
        validFace(face) = false
        allLocalFacesValid = false

      if validFace(face) then
        val sorted = values.sorted
        val key = (sorted(0), sorted(1), sorted(2))
        firstFaceForTriangle.get(key) match
          case Some(first) =>
            collector.add(TopologyIssue.DuplicateFace(first, face))
            validFace(face) = false
            duplicateFound = true
          case None =>
            firstFaceForTriangle.update(key, face)

      if validFace(face) then
        var local = 0
        while local < 3 do
          val origin = values(local)
          val target = values((local + 1) % 3)
          val key = EdgeKey.canonical(origin, target)
          edgeUses
            .getOrElseUpdate(key, mutable.ArrayBuffer.empty)
            .addOne(EdgeUse(face, origin, target))
          local += 1
    }
    collector.complete(AuditStage.LocalFaces)
    if allLocalFacesValid then collector.complete(AuditStage.DuplicateFaces)

    var edgeManifold = true
    edgeUses.foreach { (edge, uses) =>
      if uses.length > 2 then
        edgeManifold = false
        collector.add(
          TopologyIssue.NonManifoldEdge(
            edge.first,
            edge.second,
            uses.iterator.map(_.face).toVector
          )
        )
      else if checkOrientation && uses.length == 2 then
        val first = uses(0)
        val second = uses(1)
        if first.origin == second.origin then
          collector.add(
            TopologyIssue.OrientationConflict(
              edge.first,
              edge.second,
              first.face,
              second.face
            )
          )
    }
    if allLocalFacesValid && !duplicateFound then
      collector.complete(AuditStage.EdgeIncidence)
      if edgeManifold && checkOrientation then collector.complete(AuditStage.Orientation)

    if vertexCount >= 0 then
      var vertex = 0
      while vertex < vertexCount do
        if !usedVertices(vertex) then collector.add(TopologyIssue.UnusedVertex(vertex))
        vertex += 1
      if allLocalFacesValid then collector.complete(AuditStage.VertexUsage)

      auditVertexLinks(
        table,
        validFace,
        vertexCount,
        collector
      )
      if allLocalFacesValid && !duplicateFound && edgeManifold then
        collector.complete(AuditStage.VertexLinks)

    collector.result()

  private def auditVertexLinks(
      table: TriangleTable,
      validFace: Array[Boolean],
      vertexCount: Int,
      collector: AuditCollector
  ): Unit =
    val links =
      Array.fill(vertexCount)(
        mutable.ArrayBuffer.empty[(Int, Int, Int)]
      )
    table.faces.zipWithIndex.foreach { (triangle, face) =>
      if validFace(face) then
        val values = triangle.toVector
        var local = 0
        while local < 3 do
          val vertex = values(local)
          val before = values((local + 2) % 3)
          val after = values((local + 1) % 3)
          links(vertex).addOne((before, after, face))
          local += 1
    }

    var vertex = 0
    while vertex < vertexCount do
      val link = links(vertex)
      if link.nonEmpty then
        val adjacency =
          mutable.HashMap.empty[Int, mutable.ArrayBuffer[(Int, Int)]]
        link.foreach { (first, second, face) =>
          adjacency
            .getOrElseUpdate(first, mutable.ArrayBuffer.empty)
            .addOne((second, face))
          adjacency
            .getOrElseUpdate(second, mutable.ArrayBuffer.empty)
            .addOne((first, face))
        }
        val components = countComponents(adjacency)
        val degreePatternValid =
          adjacency.valuesIterator.forall(_.length <= 2) && {
            val degreeOne = adjacency.valuesIterator.count(_.length == 1)
            degreeOne == 0 || degreeOne == 2
          }
        if components != 1 || !degreePatternValid then
          collector.add(
            TopologyIssue.NonManifoldVertex(
              vertex,
              components,
              link.iterator.map(_._3).toVector.distinct.sorted
            )
          )
      vertex += 1

  private def countComponents(
      adjacency: mutable.HashMap[Int, mutable.ArrayBuffer[(Int, Int)]]
  ): Int =
    val unseen = mutable.HashSet.from(adjacency.keysIterator)
    var components = 0
    while unseen.nonEmpty do
      components += 1
      val start = unseen.head
      val stack = mutable.ArrayDeque(start)
      unseen.remove(start)
      while stack.nonEmpty do
        val current = stack.removeLast()
        adjacency(current).foreach { (next, _) =>
          if unseen.remove(next) then stack.append(next)
        }
    components

  private def compile(table: TriangleTable): TriangleTopology =
    val storage = compileStorage(table)
    val vertexDomain =
      domain("mesh vertices", table.vertexCount)
    val edgeDomain =
      domain("mesh edges", storage.edgeCount)
    val faceDomain =
      domain("mesh faces", table.faces.length)
    val halfedgeDomain =
      domain("mesh halfedges", storage.faceVertices.length)

    pack(
      vertexDomain,
      edgeDomain,
      faceDomain,
      halfedgeDomain,
      storage
    )

  private def compileOn[V](
      table: TriangleTable,
      vertices: FiniteDomain[V]
  ): TriangleTopology { type Vertex = V } =
    val storage = compileStorage(table)
    val edgeDomain =
      domain("mesh edges", storage.edgeCount)
    val faceDomain =
      domain("mesh faces", table.faces.length)
    val halfedgeDomain =
      domain("mesh halfedges", storage.faceVertices.length)
    packOn(vertices, edgeDomain, faceDomain, halfedgeDomain, storage)

  private def compileStorage(table: TriangleTable): CompiledStorage =
    val faceVertices = table.faces.iterator.flatMap(_.toVector).toArray
    val halfedgeCount = faceVertices.length
    val opposites =
      Array.fill(halfedgeCount)(TriangleTopology.BoundarySentinel)
    val edgeOfHalfedge = Array.ofDim[Int](halfedgeCount)
    val edgeForKey = mutable.LinkedHashMap.empty[EdgeKey, Int]
    val firstHalfedgeForEdge = mutable.ArrayBuffer.empty[Int]
    val unmatched = mutable.HashMap.empty[EdgeKey, Int]

    var halfedge = 0
    while halfedge < halfedgeCount do
      val origin = faceVertices(halfedge)
      val target = faceVertices(nextOrdinal(halfedge))
      val key = EdgeKey.canonical(origin, target)
      val edge =
        edgeForKey.getOrElseUpdate(
          key, {
            val ordinal = edgeForKey.size
            firstHalfedgeForEdge.addOne(halfedge)
            ordinal
          }
        )
      edgeOfHalfedge(halfedge) = edge
      unmatched.remove(key) match
        case Some(other) =>
          opposites(halfedge) = other
          opposites(other) = halfedge
        case None =>
          unmatched.update(key, halfedge)
      halfedge += 1

    val incidentFaces =
      Array.fill(table.vertexCount)(mutable.ArrayBuffer.empty[Int])
    val neighbors =
      Array.fill(table.vertexCount)(mutable.HashSet.empty[Int])
    table.faces.zipWithIndex.foreach { (triangle, face) =>
      val values = triangle.toVector
      var local = 0
      while local < 3 do
        val vertex = values(local)
        incidentFaces(vertex).addOne(face)
        neighbors(vertex).add(values((local + 1) % 3))
        neighbors(vertex).add(values((local + 2) % 3))
        local += 1
    }

    CompiledStorage(
      edgeForKey.size,
      faceVertices,
      opposites,
      edgeOfHalfedge,
      firstHalfedgeForEdge.toArray,
      incidentFaces.map(_.toArray.sorted),
      neighbors.map(_.toArray.sorted)
    )

  private def domain(name: String, size: Int): SomeFiniteDomain =
    FiniteDomain
      .ephemeral(name, size)
      .fold(
        error =>
          throw new IllegalStateException(
            s"validated topology could not create $name: ${error.message}"
          ),
        identity
      )

  private def pack(
      vertexDomain: SomeFiniteDomain,
      edgeDomain: SomeFiniteDomain,
      faceDomain: SomeFiniteDomain,
      halfedgeDomain: SomeFiniteDomain,
      storage: CompiledStorage
  ): TriangleTopology =
    new PackedTopology[
      vertexDomain.S,
      edgeDomain.S,
      faceDomain.S,
      halfedgeDomain.S
    ](
      vertexDomain.value,
      edgeDomain.value,
      faceDomain.value,
      halfedgeDomain.value,
      storage.faceVertices,
      storage.opposites,
      storage.edgeOfHalfedge,
      storage.halfedgeOfEdge,
      storage.incidentFaces,
      storage.neighbors
    )

  private def packOn[V](
      vertices: FiniteDomain[V],
      edgeDomain: SomeFiniteDomain,
      faceDomain: SomeFiniteDomain,
      halfedgeDomain: SomeFiniteDomain,
      storage: CompiledStorage
  ): TriangleTopology { type Vertex = V } =
    new PackedTopology[
      V,
      edgeDomain.S,
      faceDomain.S,
      halfedgeDomain.S
    ](
      vertices,
      edgeDomain.value,
      faceDomain.value,
      halfedgeDomain.value,
      storage.faceVertices,
      storage.opposites,
      storage.edgeOfHalfedge,
      storage.halfedgeOfEdge,
      storage.incidentFaces,
      storage.neighbors
    )

  private def packPersistent[V, E, F, H](
      vertices: FiniteDomain[V],
      edges: FiniteDomain[E],
      faces: FiniteDomain[F],
      halfedges: FiniteDomain[H],
      storage: CompiledStorage,
      record: TriangleTopologyRecord
  ): TriangleTopology {
    type Vertex = V
    type Edge = E
    type Face = F
    type Halfedge = H
  } =
    new PackedTopology[V, E, F, H](
      vertices,
      edges,
      faces,
      halfedges,
      storage.faceVertices,
      storage.opposites,
      storage.edgeOfHalfedge,
      storage.halfedgeOfEdge,
      storage.incidentFaces,
      storage.neighbors,
      Some(record)
    )

  private def nextOrdinal(halfedge: Int): Int =
    val faceStart = halfedge - halfedge % 3
    faceStart + (halfedge + 1) % 3
