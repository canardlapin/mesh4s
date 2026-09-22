package mesh4s.geometry

import java.lang.management.ManagementFactory

import mesh4s.TriangleTopology
import spatial4s.{D3, Frame}

final class SmoothingAllocationCourtSuite extends munit.FunSuite:
  test("packed smoothing allocation is independent of iteration count"):
    val bean = ManagementFactory.getThreadMXBean match
      case value: com.sun.management.ThreadMXBean
          if value.isThreadAllocatedMemorySupported =>
        value
      case _ => fail("JVM must expose per-thread allocation accounting")
    if !bean.isThreadAllocatedMemoryEnabled then bean.setThreadAllocatedMemoryEnabled(true)
    val thread = Thread.currentThread().getId()
    def allocated(body: => Unit): Long =
      val before = bean.getThreadAllocatedBytes(thread)
      body
      bean.getThreadAllocatedBytes(thread) - before
    List((100, 160), (320, 512)).foreach: (rings, columns) =>
      val (points, faces) = SmoothingFixtures.sphere(rings, columns)
      val topology = TriangleTopology
        .fromOrdinalFaces(points.length / 3, faces)
        .fold(e => fail(e.message), identity)
      val frame = Frame.named[D3]("allocation").toOption.get
      val surface =
        SurfaceRealization.fromInterleavedDoubles(topology, frame, points).toOption.get
      var plan = CoordinateSmoothing.prepare(topology)(SmoothingBoundary.Free)
      val one = SmoothingParameters.create(1, 0.1).toOption.get
      val ten = SmoothingParameters.create(10, 0.1).toOption.get
      var checksum = 0.0
      def run(parameters: SmoothingParameters): Unit =
        val result = plan.smooth(surface, parameters).fold(e => fail(e.message), identity)
        checksum += result.positions.valueAtOffset(2)
      var warmup = 0
      while warmup < 30 do
        plan = CoordinateSmoothing.prepare(topology)(SmoothingBoundary.Free)
        run(one)
        run(ten)
        warmup += 1
      val preparationBytes = allocated {
        plan = CoordinateSmoothing.prepare(topology)(SmoothingBoundary.Free)
      }
      val oneBytes = allocated { run(one) }
      val tenBytes = allocated { run(ten) }
      val bufferBytes = 2L * points.length * 8L
      assert(
        preparationBytes <= topology.vertices.size * 8L + 8192L,
        s"prepare $preparationBytes"
      )
      assert(oneBytes <= bufferBytes + 8192L, s"one $oneBytes, buffers $bufferBytes")
      assert(tenBytes <= bufferBytes + 8192L, s"ten $tenBytes, buffers $bufferBytes")
      assert(checksum > 0.0)
      println(
        s"SMOOTHING_ALLOCATION vertices=${topology.vertices.size} preparationBytes=$preparationBytes oneIterationBytes=$oneBytes tenIterationBytes=$tenBytes packedBufferBytes=$bufferBytes"
      )
