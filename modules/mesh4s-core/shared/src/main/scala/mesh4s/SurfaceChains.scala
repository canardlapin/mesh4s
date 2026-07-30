package mesh4s

import locus4s.Index

final class SurfaceChains[T <: TriangleTopology] private[mesh4s] (
    val topology: T
):
  def foreachEdgeBoundary(
      edge: Index[topology.Edge]
  )(f: (Index[topology.Vertex], Int) => Unit): Unit =
    val endpoints = topology.endpointsOf(edge)
    f(endpoints.first, -1)
    f(endpoints.second, 1)

  def foreachFaceBoundary(
      face: Index[topology.Face]
  )(f: (Index[topology.Edge], Int) => Unit): Unit =
    topology.foreachFaceHalfedge(face) { halfedge =>
      val origin = topology.origin(halfedge)
      val target = topology.target(halfedge)
      val coefficient =
        if origin.ordinal < target.ordinal then 1 else -1
      f(topology.edgeOf(halfedge), coefficient)
    }
