package mesh4s.graph4s

import graph4s.algorithms.{PreparedShortestPaths, ShortestPathBuildError}
import locus4s.{FiniteDomain, Index}
import locus4s.data.Field
import mesh4s.TriangleTopology

enum MeshShortestPathsError:
  case Projection(error: GraphProjectionError)
  case Costs(error: ShortestPathBuildError)
  case WrongEdgeDomain

  def message: String = this match
    case Projection(error) => error.message
    case Costs(error)      => error.message
    case WrongEdgeDomain   => "edge costs belong to another topology owner"

/** Exact vertex-domain lookup: None is disconnected; Some(+Infinity) is overflow. */
final class MeshPathDistances[S] private[graph4s] (
    val vertices: FiniteDomain[S],
    private val lookup: Index[S] => Option[Double]
):
  def distanceTo(vertex: Index[S]): Option[Double] = lookup(vertex)

/** Typed adapter only: graph4s owns the generic nonnegative shortest-path engine. These are
  * graph costs, not a certification of intrinsic triangle lengths.
  */
final class MeshShortestPaths[T <: TriangleTopology] private[graph4s] (
    val topology: T
)(
    val projection: PrimalGraphProjection[topology.type]
)(
    private val prepared: PreparedShortestPaths[Index[
      topology.Vertex
    ], projection.graph.type]
):
  def distancesFrom(source: Index[topology.Vertex]): MeshPathDistances[topology.Vertex] =
    val distances = prepared.distancesFrom(projection.graphVertex(source))
    new MeshPathDistances(
      topology.vertices,
      target => distances.distanceTo(projection.graphVertex(target))
    )

object MeshShortestPaths:
  /** Snapshots costs once, preserving every vertex, edge, and declared component. */
  def create[T <: TriangleTopology](topology: T)(
      costs: Field[topology.Edge, Double]
  ): Either[MeshShortestPathsError, MeshShortestPaths[topology.type]] =
    if !topology.edges.sameRuntimeOwnerAs(costs.space) then
      Left(MeshShortestPathsError.WrongEdgeDomain)
    else
      for
        projection <- GraphProjections
          .primal(topology)
          .left
          .map(MeshShortestPathsError.Projection.apply)
        prepared <- PreparedShortestPaths
          .create(projection.graph)(edge => costs(projection.meshEdge(edge)))
          .left
          .map(MeshShortestPathsError.Costs.apply)
      yield new MeshShortestPaths[topology.type](topology)(projection)(prepared)
