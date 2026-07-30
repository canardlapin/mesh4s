# Topology and incidence

`TriangleTopology` owns four finite domains: vertices, edges, faces, and
face-halfedges. A face-halfedge is one directed edge occurrence in one
triangle. Every face has exactly three of them.

The trusted 0.1 topology is an immutable oriented triangular 2-manifold. It may
have boundary and may contain several connected components. Every edge has one
incident face-halfedge at a boundary or two oppositely directed face-halfedges
in the interior. Every vertex link is one cycle in the interior or one path at
a boundary.

Construction will reject duplicate faces, edges with more than two incident
faces, inconsistent winding, bow-tie vertex links, and unused vertices. Repair
is never a constructor option. Orientation and compaction are separately named
operations that return evidence describing what changed.

The representation uses exactly three face-halfedges per face. A boundary does
not create synthetic exterior halfedges, so `opposite` returns `None` at a
boundary.
