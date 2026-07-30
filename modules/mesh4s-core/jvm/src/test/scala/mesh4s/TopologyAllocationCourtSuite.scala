package mesh4s

import java.lang.management.ManagementFactory

import locus4s.Index

final class TopologyAllocationCourtSuite extends munit.FunSuite:
  test("primitive neighbor traversal has constant allocation"):
    val topology = grid(128, 128)
    var checksum = 0L
    val consume: Index[topology.Vertex] => Unit =
      neighbor => checksum += neighbor.ordinal.toLong

    var warmup = 0
    while warmup < 50 do
      traverseNeighbors(topology, consume)
      warmup += 1

    val allocated = allocatedBytes:
      var pass = 0
      while pass < 20 do
        traverseNeighbors(topology, consume)
        pass += 1

    assert(checksum > 0L)
    assert(
      allocated <= 4_096L,
      s"primitive neighbor traversal allocated $allocated bytes"
    )

  private def traverseNeighbors(
      topology: TriangleTopology,
      consume: Index[topology.Vertex] => Unit
  ): Unit =
    var ordinal = 0
    while ordinal < topology.vertices.size do
      topology.foreachNeighbor(
        topology.vertices.indexAtValidatedOrdinal(ordinal)
      )(consume)
      ordinal += 1

  private def grid(rows: Int, columns: Int): TriangleTopology =
    val faces = Vector.newBuilder[Triangle[Int]]
    var row = 0
    while row < rows - 1 do
      var column = 0
      while column < columns - 1 do
        val first = row * columns + column
        val second = first + 1
        val third = first + columns
        val fourth = third + 1
        faces += Triangle(first, second, fourth)
        faces += Triangle(first, fourth, third)
        column += 1
      row += 1
    TriangleTopology
      .fromOrdinalFaces(rows * columns, faces.result())
      .fold(audit => fail(audit.message), identity)

  private def allocatedBytes(body: => Unit): Long =
    ManagementFactory.getThreadMXBean match
      case bean: com.sun.management.ThreadMXBean if bean.isThreadAllocatedMemorySupported =>
        if !bean.isThreadAllocatedMemoryEnabled then
          bean.setThreadAllocatedMemoryEnabled(true)
        val thread = Thread.currentThread().getId()
        val before = bean.getThreadAllocatedBytes(thread)
        body
        bean.getThreadAllocatedBytes(thread) - before
      case _ =>
        fail("the JVM does not expose per-thread allocation accounting")
