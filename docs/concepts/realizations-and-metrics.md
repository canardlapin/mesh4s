# Realizations and intrinsic metrics

A triangle table should not be copied merely because the same surface has
white, pial, inflated, or spherical coordinates. Construct one topology, then
attach any number of frame-owned realizations and ordinary locus4s fields:

```scala mdoc
import locus4s.data.VectorField
import mesh4s.*
import mesh4s.geometry.*
import spatial4s.*

val topology =
  TriangleTopology
    .fromOrdinalFaces(
      3,
      Vector(Triangle(0, 1, 2))
    )
    .fold(audit => sys.error(audit.message), identity)

val surfaceRas =
  Frame.named[D3]("surface RAS")
    .fold(error => sys.error(error.message), identity)
val inflatedFrame =
  Frame.named[D3]("inflated display coordinates")
    .fold(error => sys.error(error.message), identity)

val white =
  SurfaceRealization
    .fromInterleavedDoubles(
      topology,
      surfaceRas,
      Array(
        0.0, 0.0, 0.0,
        3.0, 0.0, 0.0,
        0.0, 4.0, 0.0
      )
    )
    .fold(error => sys.error(error.message), identity)

val inflated =
  SurfaceRealization
    .fromInterleavedDoubles(
      topology,
      inflatedFrame,
      Array(
        0.0, 0.0, 1.0,
        2.5, 0.5, 1.0,
        -0.5, 3.5, 1.0
      )
    )
    .fold(error => sys.error(error.message), identity)

val thickness =
  VectorField
    .fromValues(topology.vertices, Vector(2.1, 2.4, 1.9))
    .fold(error => sys.error(error.message), identity)

val vertex =
  topology.vertices.index(1)
    .fold(error => sys.error(error.message), identity)

(white.position(vertex).coordinates,
 inflated.position(vertex).coordinates,
 thickness(vertex))
```

`SurfaceRealization` means only finite coordinates over the exact vertex
owner. It does not claim that triangles have positive area or that the
piecewise-linear surface is injective and free of self-intersections.
Coordinates are copied into packed float or double storage. Primitive D2 and
D3 traversal exposes coordinates without constructing a `Point` per vertex;
`position` remains available when the semantic point value is useful.

The `Frame` above is `spatial4s.Frame`, a coordinate frame. It is unrelated to
the `frame4s` typed dataframe library.

## Certify only what an algorithm needs

Unit normals and an induced intrinsic metric require nondegenerate triangles.
Certification takes an explicit area policy and also checks strict triangle
inequalities on the stored edge-length `Double` values:

```scala mdoc
val certified =
  white
    .requireNondegenerate(NondegeneracyTolerance.scaleAware)
    .fold(report => sys.error(report.message), identity)

val face =
  certified.realization.topology.faces.index(0)
    .fold(error => sys.error(error.message), identity)
val metricFace =
  certified.metric.topology.faces.index(0)
    .fold(error => sys.error(error.message), identity)

certified.metric.unit
certified.metric.edgeLengths.toVector.sorted
certified.metric.faceArea(metricFace)
SurfaceGeometry3
  .unitFaceNormal(certified, face)
  .coordinates
```

The direct `PiecewiseEuclideanMetric` constructor performs the same exact
stored-value triangle-inequality check and carries a coordinate unit even when
no ambient positions exist. A separate conditioning policy can demand a
larger relative slack; it does not weaken the metric laws.
`NondegeneracyTolerance.scaleAware` requires area greater than
`1e-12 * maxEdgeLengthSquared` and has no absolute-area floor.

D2 signed area and D3 vector area are explicitly named. Barycentric evaluation
on a certified realization returns the orthogonal off-plane residual instead
of silently discarding it. Vertex-normal averaging, curvature estimators,
smoothing, and self-intersection policy remain outside the 0.1 basic-geometry
contract.
