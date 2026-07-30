package mesh4s.geometry

import java.lang.management.ManagementFactory

import locus4s.FiniteDomain
import spatial4s.D3
import spatial4s.Frame

final class PointFieldAllocationCourtSuite extends munit.FunSuite:
  test("primitive D3 traversal allocates no point per vertex"):
    val packed =
      FiniteDomain
        .ephemeral("point-field-allocation", 100_000)
        .fold(error => fail(error.message), identity)
    val domain = packed.value
    val frame =
      Frame
        .named[D3]("allocation-frame")
        .fold(
          error => fail(error.message),
          identity
        )
    val coordinates =
      Array.tabulate(domain.size * 3)(offset => (offset % 101).toDouble)
    val field =
      PointField
        .fromInterleavedDoubles(domain, frame, coordinates)
        .fold(error => fail(error.message), identity)
    val (checksum, allocated) = traversalAllocation(field)

    assert(checksum > 0.0)
    assert(
      allocated <= 4_096L,
      s"2,000,000 packed D3 vertex visits allocated $allocated bytes"
    )

  private def traversalAllocation[S](
      field: PointField[S, D3, Frame[D3]]
  ): (Double, Long) =
    var checksum = 0.0
    val consume: D3PointConsumer[S] =
      (_, x, y, z) => checksum += x + y + z

    var warmup = 0
    while warmup < 50 do
      field.foreachD3(consume)
      warmup += 1

    val allocated = allocatedBytes:
      var pass = 0
      while pass < 20 do
        field.foreachD3(consume)
        pass += 1

    (checksum, allocated)

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
