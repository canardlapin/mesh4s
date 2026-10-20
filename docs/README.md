# mesh4s

Use mesh4s when one triangle table has several scientifically distinct
coordinate realizations—white, pial, inflated, and spherical—and data must
remain attached to the exact surface vertices:

```scala mdoc
import locus4s.data.VectorField
import mesh4s.*
import mesh4s.geometry.*
import spatial4s.{D3, Frame}

val topology =
  TriangleTopology
    .fromOrdinalFaces(
      vertexCount = 3,
      faces = Vector(Triangle(0, 1, 2))
    )
    .fold(audit => sys.error(audit.message), identity)

def frame(name: String) =
  Frame.named[D3](name)
    .fold(error => sys.error(error.message), identity)

val surfaceRas = frame("surface RAS")
val inflatedFrame = frame("inflated display")
val sphericalFrame = frame("spherical registration")

def realize(frame: Frame[D3], z: Double) =
  SurfaceRealization
    .fromInterleavedDoubles(
      topology,
      frame,
      Array(
        0.0, 0.0, z,
        3.0, 0.0, z,
        0.0, 4.0, z
      )
    )
    .fold(error => sys.error(error.message), identity)

val white = realize(surfaceRas, 0.0)
val pial = realize(surfaceRas, 1.0)
val inflated = realize(inflatedFrame, 2.0)
val sphere = realize(sphericalFrame, 3.0)

val thickness =
  VectorField
    .fromValues(topology.vertices, Vector(2.1, 2.4, 1.9))
    .fold(error => sys.error(error.message), identity)
val vertex =
  topology.vertices.index(1)
    .fold(error => sys.error(error.message), identity)

(white.position(vertex).coordinates,
 pial.position(vertex).coordinates,
 inflated.position(vertex).coordinates,
 sphere.position(vertex).coordinates,
 thickness(vertex))
```

`spatial4s.Frame` above means a typed coordinate frame. It is unrelated to the
`frame4s` typed dataframe library.

The topology owns separate vertex, edge, face, and face-halfedge domains. That
makes the following mistake a compile-time error:

```scala mdoc:compile-only
import mesh4s.TriangleTopology

def originOf(topology: TriangleTopology)(
    halfedge: topology.HalfedgeIndex
): topology.VertexIndex =
  topology.origin(halfedge)
```

```scala mdoc:fail
import mesh4s.TriangleTopology

def invalidOrigin(topology: TriangleTopology)(
    face: topology.FaceIndex
): topology.VertexIndex =
  topology.origin(face)
```

Construction is strict: it never silently flips faces, drops cells, or accepts
a nonmanifold vertex. Explicit orientation and diagnostic workflows retain
evidence of what changed.

The project is pre-0.1 and no artifact is published yet.

Continue with:

- [Topology and incidence](concepts/topology.md)
- [Realizations and intrinsic metrics](concepts/realizations-and-metrics.md)
- [Coordinate smoothing](concepts/coordinate-smoothing.md)
- [Graph projections](concepts/graph-projections.md)
- [Spatial coordinate frames](concepts/spatial-frames.md)
- [Topology records and deterministic indexing](reference/topology-record-and-indexing.md)
- [Performance, identity, and tolerance contracts](reference/performance-identity-and-tolerance.md)
- [Build and compatibility policy](reference/build-and-compatibility.md)

## Spatial lookup and spheres

`NearestVertexIndex` searches a `PointField` with branch-and-bound Euclidean
bounds, returns an index in that exact vertex domain, and resolves equal
distances by the lowest ordinal. Empty domains return `None`. Queries use the
index's frame; topology sharing does not align distinct coordinate charts.

`RadialFaceIndex.create` requires explicit nondegeneracy admission and never
repairs, drops, reorients, or renumbers faces. Its hierarchy follows the whole
positive origin-to-query ray, including when the query lies far beyond the
surface. The query direction is normalized by its maximum absolute component,
so its distance from the origin does not change admission. A zero direction
returns no hit; normalized determinants below 1e-15 are refused. It chooses the hit with greatest minimum barycentric weight, then
lowest face ordinal. Boundary weights down to -1e-6 are clamped and normalized;
the bounding boxes include that tolerance envelope. This is radial
interpolation geometry, not a nearest-face or first-visible-surface API.

`SphereGeometry` checks origin-centered spherical coordinates and projects to
an explicit radius in the existing frame's coordinate unit. The caller supplies
the admission tolerance. The output retains the exact vertex domain and frame;
inputs remain immutable. Mesh templates, hemisphere labels, neuroimaging
registration, and conventional radii belong downstream.

`mesh4s-graph4s` also exposes `MeshShortestPaths.create(topology)(edgeCosts)`.
It accepts an exact edge-domain `Field` of finite nonnegative graph costs,
without requiring an intrinsic triangle metric. It snapshots costs, preserves
all cells/components, and delegates single-source search to graph4s.
Source and result queries use the actual mesh vertex owner. `None` means no
path; `Some(+Infinity)` means a reachable path whose cost overflows `Double`.
All-zero costs are valid upstream; neuro-specific validation remains downstream.
