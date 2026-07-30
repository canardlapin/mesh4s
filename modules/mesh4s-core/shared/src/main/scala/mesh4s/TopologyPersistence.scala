package mesh4s

import locus4s.DomainAlignment
import locus4s.DomainAlignmentError
import locus4s.DomainRecord
import locus4s.DomainRegistry
import locus4s.DomainRestoreError
import locus4s.Index

/** Caller-supplied persistent identity for one ordered topology. */
opaque type TopologyKey = String

object TopologyKey:
  enum ParseError derives CanEqual:
    case Empty

    def message: String =
      "topology key must be non-empty"

  def parse(value: String): Either[ParseError, TopologyKey] =
    val normalized = value.trim
    if normalized.isEmpty then Left(ParseError.Empty)
    else Right(normalized)

  extension (key: TopologyKey)
    def value: String =
      key

/** Version of the public cell-ordering rules. */
enum TriangleIndexingScheme(val version: Int) derives CanEqual:
  case FaceMajorFirstOccurrenceV1 extends TriangleIndexingScheme(1)

object TriangleIndexingScheme:
  def fromVersion(version: Int): Option[TriangleIndexingScheme] =
    TriangleIndexingScheme.values.find(_.version == version)

enum TopologyRecordError derives CanEqual:
  case InvalidTopologyKey(error: TopologyKey.ParseError)
  case UnsupportedIndexingScheme(version: Int)
  case UnsupportedFingerprintScheme(version: Int)
  case InvalidFingerprint(error: TopologyFingerprint.ParseError)
  case FingerprintMismatch(
      supplied: TopologyFingerprint,
      computed: TopologyFingerprint
  )
  case FaceVertexCountNotDivisibleByThree(count: Int)
  case InvalidTopology(audit: TopologyAudit)
  case VertexDomainSizeMismatch(topologySize: Int, recordSize: Int)
  case VertexDomainIdentityMismatch

  def message: String =
    this match
      case InvalidTopologyKey(error) =>
        error.message
      case UnsupportedIndexingScheme(version) =>
        s"unsupported triangle indexing scheme version $version"
      case UnsupportedFingerprintScheme(version) =>
        s"unsupported topology fingerprint scheme version $version"
      case InvalidFingerprint(error) =>
        error.message
      case FingerprintMismatch(supplied, computed) =>
        s"topology fingerprint ${supplied.value} does not match ${computed.value}"
      case FaceVertexCountNotDivisibleByThree(count) =>
        s"face vertex ordinal count $count is not divisible by three"
      case InvalidTopology(audit) =>
        s"topology record contains ${audit.totalIssueCount} topology issue(s)"
      case VertexDomainSizeMismatch(topologySize, recordSize) =>
        s"topology has $topologySize vertices but the record has $recordSize"
      case VertexDomainIdentityMismatch =>
        "the live persistent vertex domain does not match the supplied record"

/** Versioned persistence record with one authoritative topology key.
  *
  * Edge, face, and halfedge domain records are deterministic consequences of this record
  * and therefore are not independent serialized authorities.
  */
final class TriangleTopologyRecord private (
    val topologyKey: TopologyKey,
    val vertexDomain: DomainRecord,
    val faceVertexOrdinals: Vector[Int],
    val indexingScheme: TriangleIndexingScheme,
    val fingerprintScheme: TopologyFingerprintScheme,
    val connectivityFingerprint: TopologyFingerprint,
    private[mesh4s] val edgeCount: Int
):
  def faceCount: Int =
    faceVertexOrdinals.length / 3

  def halfedgeCount: Int =
    faceVertexOrdinals.length

  def table: TriangleTable =
    TriangleTable(
      vertexDomain.size,
      faceVertexOrdinals
        .grouped(3)
        .map {
          case Seq(first, second, third) =>
            Triangle(first, second, third)
          case values =>
            throw new IllegalStateException(
              s"validated topology record contained ${values.length} trailing ordinals"
            )
        }
        .toVector
    )

  def edgeDomain: DomainRecord =
    derivedDomain(CellKind.Edge, edgeCount)

  def faceDomain: DomainRecord =
    derivedDomain(CellKind.Face, faceCount)

  def halfedgeDomain: DomainRecord =
    derivedDomain(CellKind.Halfedge, halfedgeCount)

  def samePersistentTopologyAs(that: TriangleTopologyRecord): Boolean =
    topologyKey == that.topologyKey &&
      vertexDomain.key == that.vertexDomain.key &&
      faceVertexOrdinals == that.faceVertexOrdinals &&
      indexingScheme == that.indexingScheme &&
      fingerprintScheme == that.fingerprintScheme &&
      connectivityFingerprint == that.connectivityFingerprint

  override def equals(other: Any): Boolean =
    other match
      case that: TriangleTopologyRecord =>
        topologyKey == that.topologyKey &&
        vertexDomain == that.vertexDomain &&
        faceVertexOrdinals == that.faceVertexOrdinals &&
        indexingScheme == that.indexingScheme &&
        fingerprintScheme == that.fingerprintScheme &&
        connectivityFingerprint == that.connectivityFingerprint
      case _ =>
        false

  override def hashCode(): Int =
    var result = topologyKey.hashCode()
    result = 31 * result + vertexDomain.hashCode()
    result = 31 * result + faceVertexOrdinals.hashCode()
    result = 31 * result + indexingScheme.hashCode()
    result = 31 * result + fingerprintScheme.hashCode()
    31 * result + connectivityFingerprint.hashCode()

  override def toString: String =
    s"TriangleTopologyRecord(${topologyKey.value}, " +
      s"vertices=${vertexDomain.size}, faces=$faceCount, " +
      s"fingerprint=${connectivityFingerprint.value})"

  private def derivedDomain(kind: CellKind, size: Int): DomainRecord =
    val kindName =
      kind match
        case CellKind.Edge     => "edge"
        case CellKind.Face     => "face"
        case CellKind.Halfedge => "halfedge"
        case CellKind.Vertex   =>
          throw new IllegalArgumentException(
            "the vertex domain is supplied rather than derived"
          )
    val keyValue = topologyKey.value
    val id =
      s"mesh4s:topology:v${indexingScheme.version}:${keyValue.length}:$keyValue:$kindName"
    val structural =
      s"mesh4s:${fingerprintScheme.version}:${connectivityFingerprint.value}:$kindName"
    DomainRecord
      .parse(id, s"mesh $kindName cells", size, Some(structural))
      .fold(
        error =>
          throw new IllegalStateException(
            s"validated topology record could not derive its $kindName domain: " +
              error.message
          ),
        identity
      )

object TriangleTopologyRecord:
  val CurrentIndexingScheme: TriangleIndexingScheme =
    TriangleIndexingScheme.FaceMajorFirstOccurrenceV1
  val CurrentFingerprintScheme: TopologyFingerprintScheme =
    TopologyFingerprintScheme.OrderedTrianglesV1

  def create(
      topologyKey: TopologyKey,
      vertexDomain: DomainRecord,
      faceVertexOrdinals: IterableOnce[Int]
  ): Either[TopologyRecordError, TriangleTopologyRecord] =
    validated(
      topologyKey,
      vertexDomain,
      faceVertexOrdinals.iterator.toVector,
      CurrentIndexingScheme,
      CurrentFingerprintScheme,
      None
    )

  /** Dynamic record boundary. The supplied digest is recomputed and checked. */
  def parse(
      topologyKey: String,
      vertexDomain: DomainRecord,
      faceVertexOrdinals: IterableOnce[Int],
      indexingSchemeVersion: Int,
      fingerprintSchemeVersion: Int,
      connectivityFingerprint: String
  ): Either[TopologyRecordError, TriangleTopologyRecord] =
    for
      parsedKey <-
        TopologyKey
          .parse(topologyKey)
          .left
          .map(TopologyRecordError.InvalidTopologyKey.apply)
      indexingScheme <-
        TriangleIndexingScheme
          .fromVersion(indexingSchemeVersion)
          .toRight(
            TopologyRecordError.UnsupportedIndexingScheme(indexingSchemeVersion)
          )
      fingerprintScheme <-
        TopologyFingerprintScheme
          .fromVersion(fingerprintSchemeVersion)
          .toRight(
            TopologyRecordError.UnsupportedFingerprintScheme(
              fingerprintSchemeVersion
            )
          )
      supplied <-
        TopologyFingerprint
          .parse(connectivityFingerprint)
          .left
          .map(TopologyRecordError.InvalidFingerprint.apply)
      record <-
        validated(
          parsedKey,
          vertexDomain,
          faceVertexOrdinals.iterator.toVector,
          indexingScheme,
          fingerprintScheme,
          Some(supplied)
        )
    yield record

  def fromTopology(
      topology: TriangleTopology,
      topologyKey: TopologyKey,
      vertexDomain: DomainRecord
  ): Either[TopologyRecordError, TriangleTopologyRecord] =
    if topology.vertices.size != vertexDomain.size then
      Left(
        TopologyRecordError.VertexDomainSizeMismatch(
          topology.vertices.size,
          vertexDomain.size
        )
      )
    else if topology.vertices.persistentRecord.exists(_.key != vertexDomain.key) then
      Left(TopologyRecordError.VertexDomainIdentityMismatch)
    else create(topologyKey, vertexDomain, topology.faceVertexOrdinals)

  private def validated(
      topologyKey: TopologyKey,
      vertexDomain: DomainRecord,
      ordinals: Vector[Int],
      indexingScheme: TriangleIndexingScheme,
      fingerprintScheme: TopologyFingerprintScheme,
      suppliedFingerprint: Option[TopologyFingerprint]
  ): Either[TopologyRecordError, TriangleTopologyRecord] =
    if ordinals.length % 3 != 0 then
      Left(TopologyRecordError.FaceVertexCountNotDivisibleByThree(ordinals.length))
    else
      val table =
        TriangleTable(
          vertexDomain.size,
          ordinals
            .grouped(3)
            .map(values => Triangle(values(0), values(1), values(2)))
            .toVector
        )
      TopologyBuilder
        .build(table, TopologyAudit.DefaultIssueLimit)
        .left
        .map(TopologyRecordError.InvalidTopology.apply)
        .flatMap: topology =>
          val computed =
            TopologyFingerprint.compute(
              vertexDomain.size,
              ordinals,
              fingerprintScheme
            )
          suppliedFingerprint match
            case Some(supplied) if supplied != computed =>
              Left(TopologyRecordError.FingerprintMismatch(supplied, computed))
            case _ =>
              Right(
                new TriangleTopologyRecord(
                  topologyKey,
                  vertexDomain,
                  ordinals,
                  indexingScheme,
                  fingerprintScheme,
                  computed,
                  topology.edges.size
                )
              )

enum TopologyRestoreError:
  case ConflictingTopology(
      existing: TriangleTopologyRecord,
      requested: TriangleTopologyRecord
  )
  case Domain(error: DomainRestoreError)
  case InvalidCompiledTopology(audit: TopologyAudit)

  def message: String =
    this match
      case ConflictingTopology(existing, requested) =>
        s"topology key ${requested.topologyKey.value} is already registered with " +
          s"${existing.connectivityFingerprint.value}, not " +
          requested.connectivityFingerprint.value
      case Domain(error) =>
        error.message
      case InvalidCompiledTopology(audit) =>
        s"validated topology record failed compilation with " +
          s"${audit.totalIssueCount} issue(s)"

sealed trait TopologyResolution:
  type T <: TriangleTopology
  val registry: TopologyRegistry
  val topology: T

/** Immutable threaded registry for persistent topology owners. */
final class TopologyRegistry private (
    private val entries: Map[TopologyKey, TopologyRegistry.Stored],
    val domains: DomainRegistry
):
  def restore(
      record: TriangleTopologyRecord
  ): Either[TopologyRestoreError, TopologyResolution] =
    entries.get(record.topologyKey) match
      case Some(existing) =>
        if existing.record.samePersistentTopologyAs(record) then
          Right(TopologyRegistry.resolution(this, existing.topology))
        else
          Left(
            TopologyRestoreError.ConflictingTopology(existing.record, record)
          )
      case None =>
        restoreNew(record)

  def find(key: TopologyKey): Option[TriangleTopology] =
    entries.get(key).map(_.topology)

  def size: Int =
    entries.size

  private def restoreNew(
      record: TriangleTopologyRecord
  ): Either[TopologyRestoreError, TopologyResolution] =
    domains
      .restore(record.vertexDomain)
      .left
      .map(TopologyRestoreError.Domain.apply)
      .flatMap: vertex =>
        vertex.registry
          .restore(record.edgeDomain)
          .left
          .map(TopologyRestoreError.Domain.apply)
          .flatMap: edge =>
            edge.registry
              .restore(record.faceDomain)
              .left
              .map(TopologyRestoreError.Domain.apply)
              .flatMap: face =>
                face.registry
                  .restore(record.halfedgeDomain)
                  .left
                  .map(TopologyRestoreError.Domain.apply)
                  .flatMap: halfedge =>
                    TopologyBuilder
                      .buildPersistent(
                        record,
                        vertex.space,
                        edge.space,
                        face.space,
                        halfedge.space
                      )
                      .left
                      .map(TopologyRestoreError.InvalidCompiledTopology.apply)
                      .map: topology =>
                        val updated =
                          new TopologyRegistry(
                            entries.updated(
                              record.topologyKey,
                              TopologyRegistry.stored(record, topology)
                            ),
                            halfedge.registry
                          )
                        TopologyRegistry.resolution(updated, topology)

object TopologyRegistry:
  private sealed trait Stored:
    type T <: TriangleTopology
    val record: TriangleTopologyRecord
    val topology: T

  private def stored[T0 <: TriangleTopology](
      persistenceRecord: TriangleTopologyRecord,
      value: T0
  ): Stored { type T = T0 } =
    new Stored:
      type T = T0
      val record: TriangleTopologyRecord = persistenceRecord
      val topology: T0 = value

  private def resolution[T0 <: TriangleTopology](
      updatedRegistry: TopologyRegistry,
      value: T0
  ): TopologyResolution { type T = T0 } =
    new TopologyResolution:
      type T = T0
      val registry: TopologyRegistry = updatedRegistry
      val topology: T0 = value

  val empty: TopologyRegistry =
    new TopologyRegistry(Map.empty, DomainRegistry.empty)

enum TopologyAlignmentError:
  case MissingPersistentRecord(side: String)
  case DifferentTopologyKeys(left: TopologyKey, right: TopologyKey)
  case DifferentIncidence
  case Domain(kind: CellKind, error: DomainAlignmentError)
  case CompositionOwnerMismatch

  def message: String =
    this match
      case MissingPersistentRecord(side) =>
        s"$side topology has no persistent record"
      case DifferentTopologyKeys(left, right) =>
        s"topology keys differ: ${left.value} != ${right.value}"
      case DifferentIncidence =>
        "topologies do not have identical ordered incidence"
      case Domain(kind, error) =>
        s"$kind domains do not align: ${error.message}"
      case CompositionOwnerMismatch =>
        "alignment composition requires the same intermediate runtime owner"

/** Exact ordinal-preserving correspondence between two ordered topology owners. */
sealed abstract class TopologyAlignment[
    L <: TriangleTopology,
    R <: TriangleTopology
] private[mesh4s]:
  val left: L
  val right: R
  val vertices: DomainAlignment[left.Vertex, right.Vertex]
  val edges: DomainAlignment[left.Edge, right.Edge]
  val faces: DomainAlignment[left.Face, right.Face]
  val halfedges: DomainAlignment[left.Halfedge, right.Halfedge]

  final def vertexToRight(index: Index[left.Vertex]): Index[right.Vertex] =
    vertices.toRight(index)

  final def edgeToRight(index: Index[left.Edge]): Index[right.Edge] =
    edges.toRight(index)

  final def faceToRight(index: Index[left.Face]): Index[right.Face] =
    faces.toRight(index)

  final def halfedgeToRight(
      index: Index[left.Halfedge]
  ): Index[right.Halfedge] =
    halfedges.toRight(index)

  final def vertexToLeft(index: Index[right.Vertex]): Index[left.Vertex] =
    vertices.toLeft(index)

  final def edgeToLeft(index: Index[right.Edge]): Index[left.Edge] =
    edges.toLeft(index)

  final def faceToLeft(index: Index[right.Face]): Index[left.Face] =
    faces.toLeft(index)

  final def halfedgeToLeft(
      index: Index[right.Halfedge]
  ): Index[left.Halfedge] =
    halfedges.toLeft(index)

  final def reverse: TopologyAlignment[R, L] =
    TopologyAlignment.unsafe(
      right,
      left,
      vertices.reverse,
      edges.reverse,
      faces.reverse,
      halfedges.reverse
    )

  final def andThen[C <: TriangleTopology](
      that: TopologyAlignment[R, C]
  ): Either[TopologyAlignmentError, TopologyAlignment[L, C]] =
    if right eq that.left then TopologyAlignment.alignDomains(left, that.right)
    else Left(TopologyAlignmentError.CompositionOwnerMismatch)

object TopologyAlignment:
  def identity[T <: TriangleTopology](
      topology: T
  ): TopologyAlignment[topology.type, topology.type] =
    unsafe(
      topology,
      topology,
      DomainAlignment.identity(topology.vertices),
      DomainAlignment.identity(topology.edges),
      DomainAlignment.identity(topology.faces),
      DomainAlignment.identity(topology.halfedges)
    )

  def check[L <: TriangleTopology, R <: TriangleTopology](
      left: L,
      right: R
  ): Either[TopologyAlignmentError, TopologyAlignment[L, R]] =
    if left eq right then alignDomains(left, right)
    else
      for
        leftRecord <-
          left.persistenceRecord.toRight(
            TopologyAlignmentError.MissingPersistentRecord("left")
          )
        rightRecord <-
          right.persistenceRecord.toRight(
            TopologyAlignmentError.MissingPersistentRecord("right")
          )
        _ <-
          Either.cond(
            leftRecord.topologyKey == rightRecord.topologyKey,
            (),
            TopologyAlignmentError.DifferentTopologyKeys(
              leftRecord.topologyKey,
              rightRecord.topologyKey
            )
          )
        _ <-
          Either.cond(
            TriangleTopology.sameOrderedIncidence(left, right),
            (),
            TopologyAlignmentError.DifferentIncidence
          )
        alignment <- alignDomains(left, right)
      yield alignment

  private[mesh4s] def alignDomains[
      L <: TriangleTopology,
      R <: TriangleTopology
  ](
      left: L,
      right: R
  ): Either[TopologyAlignmentError, TopologyAlignment[L, R]] =
    for
      vertexAlignment <-
        left.vertices
          .align(right.vertices)
          .left
          .map(TopologyAlignmentError.Domain(CellKind.Vertex, _))
      edgeAlignment <-
        left.edges
          .align(right.edges)
          .left
          .map(TopologyAlignmentError.Domain(CellKind.Edge, _))
      faceAlignment <-
        left.faces
          .align(right.faces)
          .left
          .map(TopologyAlignmentError.Domain(CellKind.Face, _))
      halfedgeAlignment <-
        left.halfedges
          .align(right.halfedges)
          .left
          .map(TopologyAlignmentError.Domain(CellKind.Halfedge, _))
    yield unsafe(
      left,
      right,
      vertexAlignment,
      edgeAlignment,
      faceAlignment,
      halfedgeAlignment
    )

  private def unsafe[L <: TriangleTopology, R <: TriangleTopology](
      leftTopology: L,
      rightTopology: R,
      vertexAlignment: DomainAlignment[
        leftTopology.Vertex,
        rightTopology.Vertex
      ],
      edgeAlignment: DomainAlignment[leftTopology.Edge, rightTopology.Edge],
      faceAlignment: DomainAlignment[leftTopology.Face, rightTopology.Face],
      halfedgeAlignment: DomainAlignment[
        leftTopology.Halfedge,
        rightTopology.Halfedge
      ]
  ): TopologyAlignment[L, R] =
    new TopologyAlignment[L, R]:
      val left: leftTopology.type = leftTopology
      val right: rightTopology.type = rightTopology
      val vertices: DomainAlignment[left.Vertex, right.Vertex] =
        vertexAlignment
      val edges: DomainAlignment[left.Edge, right.Edge] =
        edgeAlignment
      val faces: DomainAlignment[left.Face, right.Face] =
        faceAlignment
      val halfedges: DomainAlignment[left.Halfedge, right.Halfedge] =
        halfedgeAlignment
