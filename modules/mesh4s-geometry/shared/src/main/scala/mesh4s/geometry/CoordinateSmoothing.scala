package mesh4s.geometry

import locus4s.Index
import mesh4s.TriangleTopology
import spatial4s.Dim
import spatial4s.Frame

/** Fixed boundaries retain their coordinates; free boundaries use all neighbors. */
enum SmoothingBoundary derives CanEqual:
  case Fixed, Free

enum SmoothingError derives CanEqual:
  case InvalidIterations(value: Int)
  case InvalidStep(value: Double)
  case WrongTopology
  case NonFiniteUpdate(iteration: Int, vertex: Int, axis: Int)

  def message: String = this match
    case InvalidIterations(value) => s"iterations must be non-negative, found $value"
    case InvalidStep(value)       => s"step must be finite and in [0, 1], found $value"
    case WrongTopology            => "smoothing plan belongs to another topology owner"
    case NonFiniteUpdate(iteration, vertex, axis) =>
      s"non-finite smoothing update at iteration $iteration, vertex $vertex, axis $axis"

/** Validated explicit iteration count and convex averaging step. */
final class SmoothingParameters private (val iterations: Int, val step: Double)

object SmoothingParameters:
  def create(iterations: Int, step: Double): Either[SmoothingError, SmoothingParameters] =
    if iterations < 0 then Left(SmoothingError.InvalidIterations(iterations))
    else if !step.isFinite || step < 0.0 || step > 1.0 then
      Left(SmoothingError.InvalidStep(step))
    else Right(new SmoothingParameters(iterations, step))

/** Reusable immutable plan for one exact topology owner.
  *
  * Uniform Laplacian updates are synchronous: p' = p + step * mean(q - p). Constrained
  * vertices stay fixed. Connectivity, vertex ordering, and frame ownership are unchanged.
  * Positive runs return Float64 coordinates. This does not certify geometric validity:
  * shrinking, collapsed/inverted faces, and self-intersections remain possible. No fields
  * or normals are updated.
  */
final class CoordinateSmoothing[T <: TriangleTopology] private (
    val topology: T,
    private val weights: Array[Double]
):
  /** O(iterations * (V + E) * D) time, two packed coordinate buffers per run. Zero
    * iterations or zero step returns the input itself. Finite input is guaranteed by
    * SurfaceRealization; arithmetic overflow is a typed refusal.
    */
  def smooth[R >: T <: TriangleTopology, D <: Dim, F <: Frame[D]](
      surface: SurfaceRealization[R, D, F],
      parameters: SmoothingParameters
  ): Either[SmoothingError, SurfaceRealization[R, D, F]] =
    if !(surface.topology eq topology) then Left(SmoothingError.WrongTopology)
    else if parameters.iterations == 0 || parameters.step == 0.0 then Right(surface)
    else
      given spatial4s.Dimension[D] = surface.dimension
      val rank = surface.dimension.rank
      var current = surface.positions.interleavedDoubles
      var next = new Array[Double](current.length)
      var offset = 0
      var weight = 0.0
      // One callback per run, reused across all vertices and iterations.
      val accumulate: Index[topology.Vertex] => Unit = neighbor =>
        val source = neighbor.ordinal * rank
        var axis = 0
        while axis < rank do
          next(offset + axis) += (current(source + axis) - current(offset + axis)) * weight
          axis += 1
      var iteration = 0
      while iteration < parameters.iterations do
        var vertex = 0
        while vertex < topology.vertices.size do
          offset = vertex * rank
          weight = weights(vertex)
          var axis = 0
          while axis < rank do
            next(offset + axis) = 0.0
            axis += 1
          if weight != 0.0 then
            topology.foreachNeighbor(topology.vertices.indexAtValidatedOrdinal(vertex))(
              accumulate
            )
          axis = 0
          while axis < rank do
            val value =
              if weight == 0.0 then current(offset + axis)
              else current(offset + axis) + parameters.step * next(offset + axis)
            if !value.isFinite then
              return Left(SmoothingError.NonFiniteUpdate(iteration + 1, vertex, axis))
            next(offset + axis) = value
            axis += 1
          vertex += 1
        val swap = current
        current = next
        next = swap
        iteration += 1
      val positions = PointField.fromOwnedFiniteDoubles(
        surface.topology.vertices,
        surface.frame,
        current
      )
      Right(new SurfaceRealization[R, D, F](surface.topology, positions))

object CoordinateSmoothing:
  /** O(V + E + H) preparation, O(V) retained storage; adjacency stays in topology.
    * Constraints are copied into the plan, and always override free boundaries. All
    * topology admitted by TriangleTopology is supported, including empty and disconnected
    * surfaces. Isolated vertices remain rejected by topology admission. Degenerate
    * coordinates are allowed.
    */
  def prepare[T <: TriangleTopology](topology: T)(
      boundary: SmoothingBoundary,
      fixedVertices: IterableOnce[Index[topology.Vertex]] =
        Iterator.empty[Index[topology.Vertex]]
  ): CoordinateSmoothing[topology.type] =
    val weights = new Array[Double](topology.vertices.size)
    var count = 0
    val countNeighbor: Index[topology.Vertex] => Unit = _ => count += 1
    topology.vertices.foreachIndex: vertex =>
      count = 0
      topology.foreachNeighbor(vertex)(countNeighbor)
      if count > 0 then weights(vertex.ordinal) = 1.0 / count
    if boundary == SmoothingBoundary.Fixed then
      topology.foreachBoundaryHalfedge: halfedge =>
        weights(topology.origin(halfedge).ordinal) = 0.0
        weights(topology.target(halfedge).ordinal) = 0.0
    fixedVertices.iterator.foreach(vertex => weights(vertex.ordinal) = 0.0)
    new CoordinateSmoothing[topology.type](topology, weights)
