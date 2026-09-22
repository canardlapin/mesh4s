# Coordinate smoothing validation — 2026-09-22

Issue: `bd-01M34K2WR4XT4BR0F7NVS80ET7`.

Measurements were taken from the worktree on exact base commit
`dfcb87507a53f73c42a38af5b0fc27b4029774dc` (main and live origin/main agreed).
The file manifest below identifies the tested source committed alongside this
report. Obtain its implementation commit with
`git log -1 --format=%H -- modules/mesh4s-geometry/shared/src/main/scala/mesh4s/geometry/CoordinateSmoothing.scala`.
No library release is claimed. Unrelated untracked README and Mote operations
were preserved.

## Contract and review

Uniform, synchronous coordinate smoothing uses a validated finite step in [0, 1]
and nonnegative iteration count. Boundary policy is explicit; copied typed
constraints override free boundaries. Identity operations return the input
object. Positive runs produce immutable Float64 coordinates with the same
runtime topology/frame owners and compile-time topology/frame types. Arithmetic
overflow refuses the entire result with a typed error. Adjacency remains owned
by TriangleTopology. Isolated vertices remain rejected by its existing
`UnusedVertex` admission rule; no topology checks were weakened.

Review covered buffer swapping and ownership transfer, input immutability,
finite-output admission, statically rejected foreign constraints/realizations,
runtime refusal after type widening, and generic consumer ergonomics. An
independent face-set reference, hand-computed one/two-step fixtures, transformed
and relabeled inputs, and analytic tetrahedron area/volume shrinkage exercise
the numerical contract. No scientific fields or normals are smoothed.

## Gates

All commands exited 0, using the checked-in immutable dependency pins, Scala
3.7.4, sbt 1.11.7, Homebrew Java 25.0.1, Node v26.7.0, macOS 14.3 arm64. The
repository toolchain was not migrated as part of this feature. The hosted
JDK 17/21 matrix was not run in this local session.

```text
sbt -J-Xmx4G scalafmtAll checkAll testFullOptJS docsCheck
sbt -J-Xmx4G scalafmtAll scalafmtCheckAll \
  'mesh4s-geometryJVM/testOnly mesh4s.geometry.SmoothingScaleSuite' \
  'set Global / scalaJSStage := FullOptStage' \
  'mesh4s-geometryJS/testOnly mesh4s.geometry.SmoothingScaleSuite'
```

- `checkAll`: 159 tests passed (JVM plus FastOpt JS), no failures or ignored tests.
- `testFullOptJS`: 78 tests passed, no failures or ignored tests.
- Geometry specifically: 25 JVM tests and 23 tests in each JS mode.
- `docsCheck`: Scaladoc and executable mdoc example/site passed; 0 mdoc errors.
- Source compilation passed with `-Werror`. Scaladoc emits duplicate `-classpath`
  flag warnings across all five modules, including untouched modules. These are
  documentation-tool warnings, not a warning-free documentation claim.
- A final Scaladoc-only wording/format adjustment was followed by formatting,
  recompilation and sequential JVM/FullOpt scale checks. No kernel change followed
  the full gates.
- `git diff --check` passed.

Raw logs and exit-status/timing sidecars are retained locally in
`target/smoothing-evidence/` (ignored build artifacts): `gates-final.log`,
`final-format-timing.log`, and their `.meta.json` files. Earlier failed attempts
are retained too: topology admission correctly rejected the initial isolated
fixture, and the allocation test exposed boxing in `interleavedDoubles` before
it was changed to a primitive copying loop.

## Frozen shape budget

Synthetic corrugated sphere: 320 interior latitude rings, 512 longitude samples,
163,842 vertices and 327,680 oriented triangles. Radius is
`100 * (1 + 0.03 * sin(12 theta) * sin(10 phi) * sin(theta)^2)`.
No participant or anatomical mesh was used. The fixture represents cortical
vertex count, not cortical geometry.

Before execution the ticket recorded two iterations, step 0.1, at most 0.5%
relative area/volume change, maximum displacement at most 0.1% of the input
bounding-box diagonal, and no new degenerate faces. Independent cross products
measure area; determinant sums measure signed enclosed volume. Degeneracy means
`area <= 1e-12 * maximumSquaredEdgeLength` or nonfinite area.

| Measure | Input | Output / change |
| --- | ---: | ---: |
| Total area | 127682.2054624993 | 127671.00451493305 (−0.00877252%) |
| Signed volume | 4190078.1416756413 | 4189902.287626418 (−0.00419692%) |
| Maximum vertex displacement | 0 | 0.0379768320 |
| Bounding-box diagonal | 349.511082263 | Movement is 0.01086570% of input diagonal |
| Degenerate faces | 0 | 0 |

JVM and both JS modes agree to floating-point rounding. The budget passes, so
no shrinkage-controlled alternative was added. The coarse tetrahedron fixture
separately demonstrates substantial shrinkage: the scheme is not qualified to
preserve geometric shape on arbitrary meshes. Face inversion and
self-intersection are not ruled out by either preserved connectivity or this
local degeneracy test. No anatomical-fidelity or downstream rendering claim is
made.

## Timing and allocation

Preparation excludes topology construction and coordinate admission. Runs
include copying input coordinates, allocating two buffers, all requested
iterations and constructing the immutable result. Three warmup rounds precede
the mean of five measurements. These are local in-process measurements, not an
isolated-machine benchmark or a performance regression threshold.

| Runtime, 163,842 vertices | Prepare (ms) | One-iteration run (ms) | Two-iteration run (ms) |
| --- | ---: | ---: | ---: |
| JVM, sequential timing run | 1.671 | 5.075 | 10.886 |
| Scala.js FullOpt, sequential timing run | 11.750 | 15.334 | 47.470 |
| Scala.js FullOpt, full-gate run | 6.492 | 9.835 | 18.919 |

The JS spread makes host load/JIT/GC sensitivity visible; no stronger latency
promise is inferred. One-versus-two-run differences estimate incremental work,
not isolated per-iteration kernel timings.

JVM thread allocation after 30 warmups in the final full gate:

| Vertices | Preparation bytes | One-iteration bytes | Ten-iteration bytes |
| ---: | ---: | ---: | ---: |
| 16,002 | 128,136 | 768,248 | 768,248 |
| 163,842 | 1,310,856 | 7,864,568 | 7,864,568 |

The acceptance ceiling is `8 V + 8192` bytes for preparation and
`16 V D + 8192` bytes for each run. Thus iteration count adds no coordinate
buffers or per-vertex objects. JS heap allocation was not measured. Preparation
retains O(V) weights; run time is O(iterations * (V + E) * D).

## Tested source manifest

SHA-256 of the following newline-terminated manifest:
`e47b609ddb210be2f1a4b9c16333b0fd6806039267b360fd24b573cb01ed5fcf`.

```text
b86737846a77bfef59ba1779a23de7f0943aac2919ee4c341f5e68ca19319cf0  docs/README.md
4b066de9f3117278cb937d94fe84af42fb545429107db169ac820021f25b7738  docs/concepts/coordinate-smoothing.md
da82df42218395a4cc62660f752d25dfd7f8d0c8c44ed2d4174c3dfcea5c9c2f  modules/mesh4s-geometry/jvm/src/test/scala/mesh4s/geometry/SmoothingAllocationCourtSuite.scala
eba6ec7d2a437db8b1f7484a6ad1c4399c3c40a02f114fdf763188a84e93dcce  modules/mesh4s-geometry/shared/src/main/scala/mesh4s/geometry/CoordinateSmoothing.scala
ac30e6385dfd973d3eaa30febeac11ba8c60b6f0919e2b40ef3b24624b449334  modules/mesh4s-geometry/shared/src/main/scala/mesh4s/geometry/PointField.scala
fea062b77d961de16d63b3761f22f720e90e16d294586c3471c524a5cc9dee3b  modules/mesh4s-geometry/shared/src/main/scala/mesh4s/geometry/SurfaceRealization.scala
d8588db34e372d6da17054ba34e1c7e7c956d1c124c76f50bf8d3ad44ea76879  modules/mesh4s-geometry/shared/src/test/scala/mesh4s/geometry/CoordinateSmoothingSuite.scala
096cddfd1c2841c8852ae86b72fbe82df7329edf29830e1136786b097a82cc23  modules/mesh4s-geometry/shared/src/test/scala/mesh4s/geometry/SmoothingFixtures.scala
9e52ac38b8625bd55656dc421e94c7dc39efc64d00eb94ca3e04fdc6d55c3afe  modules/mesh4s-geometry/shared/src/test/scala/mesh4s/geometry/SmoothingOwnershipSuite.scala
cba2331932a44a68319c7d5d559118b7632f904561cbe04be5979a2d4ad48c7b  modules/mesh4s-geometry/shared/src/test/scala/mesh4s/geometry/SmoothingScaleSuite.scala
```
