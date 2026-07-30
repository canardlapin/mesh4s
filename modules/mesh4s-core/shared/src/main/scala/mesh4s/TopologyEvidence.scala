package mesh4s

final case class BoundaryReport(
    loopHalfedgeOrdinals: Vector[Vector[Int]]
)

final case class ComponentReport(
    componentVertexOrdinals: Vector[Vector[Int]]
)

final class ClosedTopology[T <: TriangleTopology] private[mesh4s] (
    val topology: T,
    val boundaryReport: BoundaryReport
)

final class ConnectedTopology[T <: TriangleTopology] private[mesh4s] (
    val topology: T,
    val componentReport: ComponentReport
)

final case class TopologySummary(
    vertexCount: Int,
    edgeCount: Int,
    faceCount: Int,
    halfedgeCount: Int,
    boundaryLoopCount: Int,
    componentCount: Int,
    eulerCharacteristic: Int
)
