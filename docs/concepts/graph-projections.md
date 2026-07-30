# Graph projections

`TriangleTopology` is not a graph with extra data. Faces, orientation, corner
identity, vertex-fan order, boundary loops, and the surface chain complex are
all lost when a surface is viewed only through adjacency.

The optional `mesh4s-graph4s` module makes that loss explicit:

```scala mdoc:compile-only
import mesh4s.TriangleTopology
import mesh4s.graph4s.GraphProjections

def project(topology: TriangleTopology) =
  GraphProjections.primal(topology)
```

The primal projection contains one graph vertex for every mesh vertex and one
graph edge for every mesh edge. It retains total correspondence in both
directions, so an algorithm result can be related to the originating mesh
cells. The interior-dual projection contains one graph vertex per face and one
graph edge per interior mesh edge. Boundary edges deliberately have no dual
graph edge.

Topology-only and metric-weighted projections have different names. A weighted
primal graph carries explicitly supplied intrinsic edge lengths, but it does
not become a surface Laplace--Beltrami discretization. Surface FEM and DEC
operators generally require both stiffness and mass structure and belong in a
future operator module.
