# Build and compatibility

mesh4s targets Scala 3.7.4 on JVM and Scala.js. The build pins source
dependencies to immutable revisions. Coordinated development may replace a pin
with an explicit repository-local system property:

| Dependency | Override property |
| --- | --- |
| locus4s | `mesh4s.locus4s.build` |
| graph4s | `mesh4s.graph4s.build` |
| spatial4s | `mesh4s.spatial4s.build` |

Run `sbt dependencyPins` to report the source selected by the current build.
The default build must not depend on an uncommitted sibling checkout.

Before 0.1, public APIs may change without binary compatibility. The published
0.1 artifacts become the MiMa baseline for later compatible releases. Any
compatibility exclusion after that release requires a repository rationale.

The primary verification commands are:

```text
sbt checkAll
sbt testFullOptJS
sbt docsCheck
```
