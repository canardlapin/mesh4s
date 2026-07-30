# mesh4s

mesh4s constructs a trusted triangular surface from finite vertex identity and
oriented triangle incidence. Use it when a face, edge, or vertex must remain
attached to one exact topology rather than being represented by an unowned
integer.

The project is pre-0.1. No published artifact is available yet. The first
implemented contract is the topology owner and its cell-specific index types:

```scala mdoc:compile-only
import mesh4s.TriangleTopology

def originOf(topology: TriangleTopology)(
    halfedge: topology.HalfedgeIndex
): topology.VertexIndex =
  topology.origin(halfedge)
```

A face index cannot be used where a halfedge belongs:

```scala mdoc:fail
import mesh4s.TriangleTopology

def invalidOrigin(topology: TriangleTopology)(
    face: topology.FaceIndex
): topology.VertexIndex =
  topology.origin(face)
```

Continue with:

- [Topology and incidence](concepts/topology.md)
- [Spatial coordinate frames](concepts/spatial-frames.md)
- [Topology records and deterministic indexing](reference/topology-record-and-indexing.md)
- [Build and compatibility policy](reference/build-and-compatibility.md)
