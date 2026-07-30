# Topology records and deterministic indexing

Two independently restored surfaces can have the same triangle rows and the
same SHA-256 fingerprint while still owning different runtime indices. A hash
is evidence that their bytes agree. It is not permission to pass a vertex from
one owner to the other.

Restore each record through a registry, then construct an exact alignment:

```scala mdoc
import locus4s.DomainRecord
import mesh4s.*

val patch =
  TriangleTopology
    .fromOrdinalFaces(
      4,
      Vector(
        Triangle(0, 1, 2),
        Triangle(0, 2, 3)
      )
    )
    .fold(audit => sys.error(audit.message), identity)

val vertexRecord =
  DomainRecord
    .parse(
      "subject-01/cortex/vertices",
      "subject 01 cortical vertices",
      4,
      Some("subject-01-cortex-v1")
    )
    .fold(error => sys.error(error.message), identity)

val topologyKey =
  TopologyKey
    .parse("subject-01/cortex/topology")
    .fold(error => sys.error(error.message), identity)

val record =
  patch
    .toRecord(topologyKey, vertexRecord)
    .fold(error => sys.error(error.message), identity)

val leftResolution =
  TopologyRegistry.empty
    .restore(record)
    .fold(error => sys.error(error.message), identity)
val rightResolution =
  TopologyRegistry.empty
    .restore(record)
    .fold(error => sys.error(error.message), identity)

val left = leftResolution.topology
val right = rightResolution.topology
val alignment =
  left.align(right)
    .fold(error => sys.error(error.message), identity)

val leftVertex =
  left.vertices.index(2)
    .fold(error => sys.error(error.message), identity)

alignment.vertexToRight(leftVertex).ordinal
```

The two calls start from independent empty registries, so `left` and `right`
are independent owners. Passing `leftVertex` directly to a `right` operation
does not compile. The alignment checks topology identity, full ordered
incidence, and all four locus4s domain identities once; its transport methods
then preserve ordinals in O(1).

## Deterministic cell ordering

Public cell ordering is part of the persistence contract:

1. Vertex order is the supplied finite-domain order.
2. Face order is the input triangle-row order.
3. Halfedge order is face-major with local corners zero through two.
4. Edge order is the first face-major occurrence of each unoriented edge.
5. Signed chain incidence orients each edge from its lower vertex ordinal to
   its higher ordinal.

Hash-map iteration must never determine a public ordinal.

The current in-memory compiler follows these rules directly. A bounded
`TopologyAudit` records concrete witnesses, the total issue count, truncation,
and which validation stages completed. Invalid local rows therefore cannot
produce a report that falsely claims edge, orientation, or vertex-link
validation completed.

## Record authority

A `TriangleTopologyRecord` contains:

- one caller-supplied `TopologyKey`;
- the existing persistent vertex `DomainRecord`;
- the ordered face vertex ordinals;
- the indexing and fingerprint scheme versions; and
- the computed connectivity fingerprint.

The topology key is the sole independent identity for mesh-owned cells. Edge,
face, and halfedge domain IDs and fingerprints are derived deterministically
from the topology key, scheme versions, connectivity fingerprint, and cell
kind. They are not serialized as three additional authorities.

`TriangleTopologyRecord.create` computes the fingerprint. The dynamic
`TriangleTopologyRecord.parse` boundary accepts serialized versions and a
digest, validates the triangle table, recomputes the digest, and rejects a
mismatch. The record cannot represent a weakened or partially valid trusted
topology.

## Canonical fingerprint bytes

Fingerprint scheme 1 hashes this big-endian byte stream:

1. one signed 32-bit scheme version;
2. one signed 32-bit vertex count; and
3. three signed 32-bit vertex ordinals for every face, in row order.

The remaining byte count determines the face count, so it is not encoded
again. The SHA-256 implementation is shared Scala code, and JVM and Scala.js
are tested against the same golden vectors.

The byte stream excludes runtime owner tokens, `TopologyKey`, `DomainKey`,
registry state, names, and other display metadata. Therefore two ephemeral,
independently owned topologies with the same vertex count and ordered rows have
the same fingerprint.

## Pure restoration and explicit comparison

`TopologyRegistry` is an immutable threaded value. A first restoration returns
a new registry and a new topology owner. Restoring the same record through
that returned registry returns the same canonical owner. Starting from another
empty registry creates an independent owner. A conflicting record under an
existing topology key is rejected.

Ordinary topology equality remains O(1) reference identity.
`sameConnectivity` is an explicitly linear comparison of vertex count and
ordered triangle rows. `align` additionally requires persistent topology
identity, validates all compiled incidence, and returns vertex, edge, face, and
halfedge `DomainAlignment` values. Automatic topology-isomorphism search is
outside the 0.1 contract.

Convenience values such as `Region`, `Cycle`, `Path`, and component vectors
materialize collections. `foreachNeighbor`, `foreachIncidentFace`,
`foreachFaceHalfedge`, and `foreachBoundaryHalfedge` remain the primitive
traversal boundary.
