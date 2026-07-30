# Performance, identity, and tolerance contracts

This page separates public guarantees from current implementation choices.
Complexity is stated in terms of the cells actually visited. A materializing
operation returns a new collection or geometric value; primitive traversal
invokes a caller-supplied consumer without constructing one typed cell object
per visit.

## Topology operations

| Operation | Cost | Materialization |
| --- | --- | --- |
| `origin`, `target`, `next`, `previous`, `faceOf`, `edgeOf` | O(1) | No typed cell object |
| `opposite` | O(1) | Returns `Option` |
| `verticesOf`, `endpointsOf` | O(1) | One small product value |
| `neighbors`, `incidentFaces` | O(degree) | A `Region` |
| `fan` | O(degree) | A `Path` or rotation-invariant `Cycle` |
| `boundaryLoops` | O(H) | All boundary cycles |
| `vertexComponents` | O(V + E) | One region per component |
| `summary` | O(V + E + H) | Boundary and component summaries |
| `foreachNeighbor`, `foreachIncidentFace` | O(degree) | No cell object per callback |
| `foreachFaceHalfedge` | O(1) | No cell object per callback |
| `foreachBoundaryHalfedge` | O(H) | No cell object per callback |

The no-cell-object traversal guarantee is tested on the JVM with per-thread
allocation accounting and on Scala.js FullOpt with an exact traversal
checksum. The current compiler uses primitive face-major arrays and does not
retain a graph or a second adjacency authority. Those arrays are an
implementation choice; deterministic public cell ordering is a contract.

## Coordinate and geometry operations

`PointField` stores one frame token and packed float or double coordinates.
`foreachD2` and `foreachD3` use the primitive `D2PointConsumer` and
`D3PointConsumer` interfaces. They do not construct a `Point`, boxed coordinate
tuple, or typed index object per vertex. `point`, `position`, `edgeVector`, and
`faceCentroid` materialize semantic geometry values. `interleavedDoubles` and
`interleavedFloats` return new packed arrays.

Scalar geometry queries are proportional only to the local cell dimension.
`boundingBox` visits every vertex and materializes its result vectors.
`requireNondegenerate` visits every edge and face and retains edge-length and
face-area fields as evidence.

## Identity and comparison

Ordinary equality is runtime owner identity and O(1). It never performs a
hidden whole-topology comparison.

- `sameConnectivity` compares the vertex count and ordered face rows in O(F).
- `connectivityFingerprint` hashes the canonical structural bytes and carries
  no owner authority.
- `align` requires matching persistent topology identity and full ordered
  incidence, then returns O(1) ordinal transport for all four cell domains.
- Automatic topology-isomorphism search is not present in 0.1.

Two independently restored owners remain distinct until an explicit alignment
is constructed, even when their fingerprints match.

## Numerical policies

Topology construction has no floating-point epsilon.

Coordinate construction requires every stored value to be finite.
`NondegeneracyTolerance` certifies a face only when

```text
area > max(absoluteArea, relativeArea * maximumSquaredEdgeLength)
```

The named `scaleAware` policy uses zero absolute area and relative area
`1e-12`. Callers may construct another finite, non-negative policy.

`PiecewiseEuclideanMetric` requires positive finite stored `Double` lengths and
checks strict triangle inequalities on those stored values. It does not apply
a hidden tolerance. `MetricConditioningPolicy` is a separate, explicit
relative-slack test for algorithms that need triangles farther from numerical
collapse.

Barycentric coordinates require nondegeneracy evidence and return an
off-plane residual rather than silently accepting a displaced query.
Self-intersection, curvature, vertex-normal weighting, and geodesic policy are
not implied by these local contracts.
