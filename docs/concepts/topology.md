# Topology and incidence

`TriangleTopology` owns four finite domains: vertices, edges, faces, and
face-halfedges. A face-halfedge is one directed edge occurrence in one
triangle. Every face has exactly three of them.

The trusted 0.1 topology is an immutable oriented triangular 2-manifold. It may
have boundary and may contain several connected components. Every edge has one
incident face-halfedge at a boundary or two oppositely directed face-halfedges
in the interior. Every vertex link is one cycle in the interior or one path at
a boundary.

Construction rejects duplicate faces, edges with more than two incident
faces, inconsistent winding, bow-tie vertex links, and unused vertices. Repair
is never a constructor option. Orientation and compaction are separately named
operations that return evidence describing what changed.

The representation uses exactly three face-halfedges per face. A boundary does
not create synthetic exterior halfedges, so `opposite` returns `None` at a
boundary.

```scala mdoc
import mesh4s.*

val patch =
  TriangleTopology
    .fromOrdinalFaces(
      vertexCount = 4,
      faces = Vector(
        Triangle(0, 1, 2),
        Triangle(0, 2, 3)
      )
    )
    .fold(audit => sys.error(audit.message), identity)

val vertex0 =
  patch.vertices.index(0)
    .fold(error => sys.error(error.message), identity)

patch.neighbors(vertex0).ordinalsInDomainOrder.toVector
patch.boundaryLoops.map(_.values.map(_.ordinal))
patch.eulerCharacteristic
```

An interior one-ring is a `Cycle`; a boundary one-ring is a `Path`. The
distinction prevents a boundary fan from masquerading as cyclic and prevents
an arbitrary first element from becoming part of an interior ring's meaning.

The strict constructor never changes input winding. An explicit orienter
preserves the first face of every edge-connected face component and returns a
face field recording every flip:

```scala mdoc
val oriented =
  TriangleTopology
    .orientAndBuild(
      TriangleTable(
        4,
        Vector(
          Triangle(0, 1, 2),
          Triangle(0, 1, 3)
        )
      )
    )
    .fold(error => sys.error(error.message), identity)

oriented.flipped.toVector
```

The chain view gives JVM-safe, separately named edge and face boundary
callbacks. Edges use increasing vertex ordinal as their canonical orientation,
and `boundary1(boundary2(face))` is exactly zero.
