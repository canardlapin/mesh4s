# Topology records and deterministic indexing

Public cell ordering is part of the persistence contract:

1. Vertex order is the supplied finite-domain order.
2. Face order is the input triangle-row order.
3. Halfedge order is face-major with local corners zero through two.
4. Edge order is the first face-major occurrence of each unoriented edge.
5. Signed chain incidence orients each edge from its lower vertex ordinal to
   its higher ordinal.

Hash-map iteration must never determine a public ordinal.

A versioned topology record contains one caller-supplied topology key, the
vertex-domain record, ordered face vertex ordinals, the indexing-scheme
version, and a connectivity fingerprint. Face, edge, and halfedge identities
are derived from that record rather than supplied as independent authorities.

The connectivity fingerprint is a structural SHA-256 digest of the fingerprint
scheme version, vertex count, and ordered triangle rows. It excludes runtime
owner tokens, topology and domain keys, registry state, and display metadata.
Matching fingerprints do not authorize index transport; exact alignment still
checks persistent identity and the full ordered incidence.
