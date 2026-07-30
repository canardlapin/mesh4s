package mesh4s

import locus4s.FiniteDomain
import locus4s.Index

/** A trusted immutable oriented triangular 2-manifold.
  *
  * Concrete instances own four finite domains. Once an ordinal has entered one of those
  * domains, local incidence queries are total. Construction, validation, and primitive
  * storage are introduced by the topology epic.
  */
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
