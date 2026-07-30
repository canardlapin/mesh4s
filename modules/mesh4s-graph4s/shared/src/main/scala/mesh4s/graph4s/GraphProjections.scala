package mesh4s.graph4s

import cats.Hash
import graph4s.Graph
import graph4s.Link
import graph4s.indexed.IndexedGraph
import graph4s.indexed.VertexOrder
import locus4s.Index
import mesh4s.TriangleTopology
import mesh4s.geometry.PiecewiseEuclideanMetric

enum GraphProjectionError:
  case GraphBuild(messages: Vector[String])
  case IndexBuild(messages: Vector[String])
  case InconsistentCorrespondence(detail: String)
  case WrongMetricTopology

  def message: String =
    this match
      case GraphBuild(messages) =>
        messages.mkString("graph construction failed: ", "; ", "")
      case IndexBuild(messages) =>
        messages.mkString("graph indexing failed: ", "; ", "")
      case InconsistentCorrespondence(detail) =>
        detail
      case WrongMetricTopology =>
        "metric belongs to another topology owner"

/** The topology-forgetting primal graph and total cell correspondence. */
final class PrimalGraphProjection[T <: TriangleTopology] private[mesh4s] (
    val topology: T,
    val graph: IndexedGraph[Index[topology.Vertex]],
    private val graphVertexByMeshOrdinal: Vector[graph.Vertex],
    private val graphEdgeByMeshOrdinal: Vector[graph.Edge],
    private val meshEdgeByGraphOrdinal: Vector[Index[topology.Edge]]
):
  def graphVertex(
      vertex: Index[topology.Vertex]
  ): graph.Vertex =
    graphVertexByMeshOrdinal(vertex.ordinal)

  def meshVertex(vertex: graph.Vertex): Index[topology.Vertex] =
    graph.label(vertex)

  def graphEdge(edge: Index[topology.Edge]): graph.Edge =
    graphEdgeByMeshOrdinal(edge.ordinal)

  def meshEdge(edge: graph.Edge): Index[topology.Edge] =
    meshEdgeByGraphOrdinal(graph.edgeOrdinal(edge))

/** The face-adjacency dual restricted to interior mesh edges. */
final class InteriorDualGraphProjection[T <: TriangleTopology] private[mesh4s] (
    val topology: T,
    val graph: IndexedGraph[Index[topology.Face]],
    private val graphVertexByFaceOrdinal: Vector[graph.Vertex],
    private val graphEdgeByMeshEdgeOrdinal: Vector[Option[graph.Edge]],
    private val meshEdgeByGraphOrdinal: Vector[Index[topology.Edge]]
):
  def graphVertex(face: Index[topology.Face]): graph.Vertex =
    graphVertexByFaceOrdinal(face.ordinal)

  def meshFace(vertex: graph.Vertex): Index[topology.Face] =
    graph.label(vertex)

  def graphEdge(edge: Index[topology.Edge]): Option[graph.Edge] =
    graphEdgeByMeshEdgeOrdinal(edge.ordinal)

  def meshInteriorEdge(edge: graph.Edge): Index[topology.Edge] =
    meshEdgeByGraphOrdinal(graph.edgeOrdinal(edge))

/** Explicit metric weights over a separately named primal projection. */
final class MetricWeightedPrimalProjection[T <: TriangleTopology] private[mesh4s] (
    val primal: PrimalGraphProjection[T],
    val metric: PiecewiseEuclideanMetric[? <: TriangleTopology],
    private val weightsByGraphOrdinal: Vector[Double]
):
  def weight(edge: primal.graph.Edge): Double =
    weightsByGraphOrdinal(primal.graph.edgeOrdinal(edge))

object GraphProjections:
  def primal[T <: TriangleTopology](
      topology: T
  ): Either[GraphProjectionError, PrimalGraphProjection[topology.type]] =
    given Hash[Index[topology.Vertex]] =
      Hash.fromUniversalHashCode
    val vertices = topology.vertices.indices.toVector
    val links =
      topology.edges.indices.map: edge =>
        val endpoints = topology.endpointsOf(edge)
        Link(endpoints.first, endpoints.second)
    for
      abstractGraph <- graph(
        Graph.of(vertices, links)
      )
      indexed <- index(
        IndexedGraph.from(
          abstractGraph,
          VertexOrder.explicit(vertices)
        )
      )
      result <- primalCorrespondence(topology, indexed)
    yield result

  def interiorDual[T <: TriangleTopology](
      topology: T
  ): Either[
    GraphProjectionError,
    InteriorDualGraphProjection[topology.type]
  ] =
    given Hash[Index[topology.Face]] =
      Hash.fromUniversalHashCode
    val faces = topology.faces.indices.toVector
    val firstHalfedge =
      Array.fill(topology.edges.size)(Option.empty[topology.HalfedgeIndex])
    topology.halfedges.foreachIndex: halfedge =>
      val edge = topology.edgeOf(halfedge)
      if firstHalfedge(edge.ordinal).isEmpty then
        firstHalfedge(edge.ordinal) = Some(halfedge)
    val links = Vector.newBuilder[Link[Index[topology.Face]]]
    var edgeOrdinal = 0
    while edgeOrdinal < firstHalfedge.length do
      firstHalfedge(edgeOrdinal)
        .flatMap(topology.opposite)
        .foreach: opposite =>
          val halfedge = firstHalfedge(edgeOrdinal).get
          links += Link(topology.faceOf(halfedge), topology.faceOf(opposite))
      edgeOrdinal += 1
    for
      abstractGraph <- graph(Graph.of(faces, links.result()))
      indexed <- index(
        IndexedGraph.from(
          abstractGraph,
          VertexOrder.explicit(faces)
        )
      )
      result <- dualCorrespondence(topology, indexed, firstHalfedge.toVector)
    yield result

  def metricWeightedPrimal[T <: TriangleTopology](
      topology: T,
      metric: PiecewiseEuclideanMetric[T]
  ): Either[
    GraphProjectionError,
    MetricWeightedPrimalProjection[topology.type]
  ] =
    if !topology.edges.sameRuntimeOwnerAs(metric.topology.edges) then
      Left(GraphProjectionError.WrongMetricTopology)
    else
      primal(topology).map: projection =>
        val weights =
          projection.graph.edges
            .map: edge =>
              val meshEdge = projection.meshEdge(edge)
              metric.length(
                metric.topology.edges.indexAtValidatedOrdinal(meshEdge.ordinal)
              )
            .toVector
        new MetricWeightedPrimalProjection(projection, metric, weights)

  private def primalCorrespondence[T <: TriangleTopology](
      topology: T,
      indexed: IndexedGraph[Index[topology.Vertex]]
  ): Either[
    GraphProjectionError,
    PrimalGraphProjection[topology.type]
  ] =
    val graphVertices =
      topology.vertices.indices.map(indexed.vertex).toVector
    val graphEdgeForKey =
      indexed.edges
        .map: edge =>
          val (firstVertex, secondVertex) = indexed.endpoints(edge)
          val first = indexed.label(firstVertex).ordinal
          val second = indexed.label(secondVertex).ordinal
          canonical(first, second) -> edge
        .toMap
    val graphEdges =
      topology.edges.indices
        .map: edge =>
          val endpoints = topology.endpointsOf(edge)
          graphEdgeForKey.get(
            canonical(endpoints.first.ordinal, endpoints.second.ordinal)
          )
        .toVector
    if graphVertices.exists(_.isEmpty) || graphEdges.exists(_.isEmpty) then
      Left(
        GraphProjectionError.InconsistentCorrespondence(
          "primal graph omitted a mesh vertex or edge"
        )
      )
    else
      val graphToMesh = Array.ofDim[Index[topology.Edge]](indexed.edgeCount)
      topology.edges.foreachIndex: edge =>
        graphToMesh(indexed.edgeOrdinal(graphEdges(edge.ordinal).get)) = edge
      Right(
        new PrimalGraphProjection[topology.type](
          topology,
          indexed,
          graphVertices.map(_.get),
          graphEdges.map(_.get),
          graphToMesh.toVector
        )
      )

  private def dualCorrespondence[T <: TriangleTopology](
      topology: T,
      indexed: IndexedGraph[Index[topology.Face]],
      firstHalfedge: Vector[Option[topology.HalfedgeIndex]]
  ): Either[
    GraphProjectionError,
    InteriorDualGraphProjection[topology.type]
  ] =
    val graphVertices =
      topology.faces.indices.map(indexed.vertex).toVector
    val graphEdgeForKey =
      indexed.edges
        .map: edge =>
          val (firstVertex, secondVertex) = indexed.endpoints(edge)
          val first = indexed.label(firstVertex).ordinal
          val second = indexed.label(secondVertex).ordinal
          canonical(first, second) -> edge
        .toMap
    val graphEdges =
      firstHalfedge.map:
        case Some(halfedge) =>
          topology
            .opposite(halfedge)
            .flatMap: opposite =>
              graphEdgeForKey.get(
                canonical(
                  topology.faceOf(halfedge).ordinal,
                  topology.faceOf(opposite).ordinal
                )
              )
        case None => None
    if graphVertices.exists(_.isEmpty) ||
      graphEdges.count(_.nonEmpty) != indexed.edgeCount
    then
      Left(
        GraphProjectionError.InconsistentCorrespondence(
          "interior dual omitted a face or interior mesh edge"
        )
      )
    else
      val graphToMesh = Array.ofDim[Index[topology.Edge]](indexed.edgeCount)
      topology.edges.foreachIndex: edge =>
        graphEdges(edge.ordinal).foreach(graphEdge =>
          graphToMesh(indexed.edgeOrdinal(graphEdge)) = edge
        )
      Right(
        new InteriorDualGraphProjection[topology.type](
          topology,
          indexed,
          graphVertices.map(_.get),
          graphEdges,
          graphToMesh.toVector
        )
      )

  private def graph[V](
      result: cats.data.ValidatedNec[graph4s.GraphBuildError[V], Graph[V]]
  ): Either[GraphProjectionError, Graph[V]] =
    result.toEither.left.map(errors =>
      GraphProjectionError.GraphBuild(errors.toNonEmptyList.toList.map(_.toString).toVector)
    )

  private def index[V](
      result: cats.data.ValidatedNec[
        graph4s.indexed.IndexingError[V],
        IndexedGraph[V]
      ]
  ): Either[GraphProjectionError, IndexedGraph[V]] =
    result.toEither.left.map(errors =>
      GraphProjectionError.IndexBuild(errors.toNonEmptyList.toList.map(_.toString).toVector)
    )

  private def canonical(first: Int, second: Int): (Int, Int) =
    if first < second then (first, second) else (second, first)
