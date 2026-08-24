package mesh4s.geometry

import java.lang.management.ManagementFactory

import locus4s.FiniteDomain
import spatial4s.D3
import spatial4s.Frame

final class PointFieldAllocationCourtSuite extends munit.FunSuite:
  test("packed D3 construction allocates only its defensive primitive copy"):
    val packed =
      FiniteDomain
        .ephemeral("point-field-construction-allocation", 100_000)
        .fold(error => fail(error.message), identity)
    val domain = packed.value
    val frame =
      Frame
        .named[D3]("construction-allocation-frame")
        .fold(
          error => fail(error.message),
          identity
        )
    val coordinates =
      Array.tabulate(domain.size * 3)(offset => (offset % 101).toDouble)

    var warmup = 0
    while warmup < 20 do
      PointField
        .fromInterleavedDoubles(domain, frame, coordinates)
        .fold(error => fail(error.message), identity)
      warmup += 1

    var field: PointField[packed.S, D3, Frame[D3]] | Null = null
    val allocated = allocatedBytes:
      field = PointField
        .fromInterleavedDoubles(domain, frame, coordinates)
        .fold(error => fail(error.message), identity)

    assert(field != null)
    val primitivePayload = coordinates.length.toLong * java.lang.Double.BYTES.toLong
    assert(
      allocated <= primitivePayload + 131_072L,
      s"packed D3 construction allocated $allocated bytes for a $primitivePayload-byte primitive payload"
    )

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
