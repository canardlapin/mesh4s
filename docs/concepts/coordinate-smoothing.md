# Coordinate smoothing

`mesh4s-geometry` provides uniform Laplacian smoothing of coordinates over one
exact `TriangleTopology`. It preserves topology ownership, vertex order, face
indices and the coordinate frame. Attached scalar/scientific fields are neither
read nor modified. A consumer can retain its fields and picking indices, create a
derived display realization, and recompute normals from that realization.

```scala mdoc
import mesh4s.{Triangle, TriangleTopology}
import mesh4s.geometry.*
import spatial4s.{D3, Frame}

val topology = TriangleTopology.fromOrdinalFaces(
  5,
  Vector(Triangle(0, 1, 2), Triangle(0, 2, 3),
         Triangle(0, 3, 4), Triangle(0, 4, 1))
).fold(error => sys.error(error.message), identity)
val frame = Frame.named[D3]("display").fold(error => sys.error(error.message), identity)
val surface = SurfaceRealization.fromInterleavedDoubles(topology, frame,
  Array(0.0, 0.0, 2.0, -1.0, -1.0, 0.0, 1.0, -1.0, 0.0,
        1.0, 1.0, 0.0, -1.0, 1.0, 0.0)
).fold(error => sys.error(error.message), identity)
val parameters = SmoothingParameters.create(iterations = 2, step = 0.1)
  .fold(error => sys.error(error.message), identity)
val plan = CoordinateSmoothing.prepare(topology)(SmoothingBoundary.Fixed)
val smoothed = plan.smooth(surface, parameters)
  .fold(error => sys.error(error.message), identity)
(smoothed.topology eq topology, smoothed.frame eq frame,
 smoothed.positions.interleavedDoubles.take(3).toVector)
```

Choose boundary behavior explicitly: `Fixed` retains all boundary vertices;
`Free` uses their full one-ring, including boundary neighbors. Pass an optional
`IterableOnce[Index[topology.Vertex]]` as `fixedVertices` to `prepare` to pin
additional vertices. These constraints override free boundaries and are copied
into the immutable plan. Isolated vertices are rejected by topology admission. A plan can be reused for
multiple realizations of its exact topology; widened types still undergo a
runtime ownership check. Independent plans/runs share no mutable working state.

## Scheme and numerical contract

For each unconstrained vertex with nonempty neighbor set N:

```text
p_next = p + step * sum((q - p) / |N| for q in N)
```

Every neighbor is read from the previous iteration (Jacobi semantics). There is
no in-place Gauss–Seidel sweep or dependency on vertex update order. Consistent
relabeling, translation, rotation and scale commute with the mathematical
operation; floating-point results can differ by rounding and summation order.
Tests use relative/absolute tolerances, not bitwise equivariance.

Iteration counts must be non-negative; steps must be finite and in `[0, 1]`.
Zero iterations or zero step returns the original realization, including storage
precision and object identity. Positive runs produce new Float64 packed
coordinates without mutating the input or caller arrays. Finite coordinates are
required by `PointField`/`SurfaceRealization` construction. Extreme finite inputs
can overflow the difference/summation arithmetic: the entire run then returns
`NonFiniteUpdate` with a one-based iteration, zero-based vertex and axis; no
partial realization is returned.

All topology accepted by `TriangleTopology` is supported, including disconnected
components, open surfaces and empty surfaces. Isolated vertices and nonmanifold
topology remain rejected by topology admission; smoothing does not repair them. Coincident vertices and degenerate
faces are permitted as coordinate inputs and may remain or become degenerate.
Call `requireNondegenerate` on the result when that evidence is needed.

In exact arithmetic each new point is a convex combination of its old position
and neighbors. It remains in the component's original convex hull. A single-step
displacement is at most `step` times the largest incident edge length at that
iteration. After k iterations, a coarse bound is `k * step * original component
diameter`. Rounding requires slack when testing those bounds.

Uniform weighting ignores edge lengths and face areas. It is suitable for a
small, explicitly assessed smoothing experiment; it is not a remesher or a
curvature flow with a geometric fidelity guarantee. Even two iterations can
shrink coarse meshes substantially: a centered regular tetrahedron scales by
`(1 - 4 * step / 3)^2`, with area and volume scaling by the square and cube of
that factor. Topology preservation does not imply preserved volume, valid face
orientation, absence of self-intersections, or unchanged anatomical meaning.
Normals and other derived geometry must be recomputed by the consumer. No
scientific sampling coordinates or application defaults are changed by this API.

## Cost and evidence

Preparation costs O(V + E + H), retaining one Double per vertex for inverse
degree/constraint weights. Adjacency is traversed from the topology rather than
copied into a second owner. Each iteration costs O((V + E) D). A run allocates two
packed Double coordinate buffers (16 V D bytes plus fixed overhead), independent
of iteration count; the final buffer becomes immutable result storage. There are
no per-vertex points, neighbor collections, or per-iteration coordinate buffers.

The shared suites check hand-computable fixtures, a separate face-set reference,
coordinate transforms and relabeling, identity, constraints, empty and rejected-isolated topology, and
nonfinite cases. A synthetic corrugated sphere with 163,842 vertices and 327,680
faces represents cortical **size**, not real cortical geometry. Its frozen budget
for two iterations at step 0.1 is <=0.5% relative area/volume change, maximum
movement <=0.1% of the input bounding-box diagonal, and no degenerate faces under
`area <= 1e-12 * maxSquaredEdgeLength`. Shape measurements use independent cross
products and signed volume determinants. Passing this fixture does not qualify
an anatomical dataset or prove absence of self-intersections.

`SmoothingScaleSuite` reports preparation, one-iteration runs and two-iteration
runs separately (three warmups, mean of five measurements). Run times include
buffer setup and result construction; subtracting them is only an estimate of
incremental iteration cost. Timing is descriptive, not a hardware-dependent CI
threshold. `SmoothingAllocationCourtSuite` uses JVM thread allocation accounting
at 16,002 and 163,842 vertices after warmup; it checks the two-buffer allocation
budget for both one and ten iterations. Shared correctness and shape courts run
on JVM and Scala.js, including FullOpt.
