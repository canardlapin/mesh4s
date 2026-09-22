package mesh4s.geometry

import mesh4s.TriangleTopology
import spatial4s.{D3, Frame}

final class SmoothingScaleSuite extends munit.FunSuite:
  test("synthetic cortical-size shape budget and separately timed preparation/runs"):
    val (points, faces) = SmoothingFixtures.sphere(320, 512)
    val topology = TriangleTopology
      .fromOrdinalFaces(points.length / 3, faces)
      .fold(e => fail(e.message), identity)
    val frame =
      Frame.named[D3]("synthetic corrugated sphere").fold(e => fail(e.message), identity)
    val surface = SurfaceRealization
      .fromInterleavedDoubles(topology, frame, points)
      .fold(e => fail(e.message), identity)
    val one = SmoothingParameters.create(1, 0.1).toOption.get
    val two = SmoothingParameters.create(2, 0.1).toOption.get
    var preparation = 0L
    var single = 0L
    var double = 0L
    // Warm the exact paths; timing is evidence, never a hardware-specific pass gate.
    var run = 0
    while run < 8 do
      val start = System.nanoTime()
      val plan = CoordinateSmoothing.prepare(topology)(SmoothingBoundary.Free)
      val prepared = System.nanoTime()
      val first = plan.smooth(surface, one).fold(e => fail(e.message), identity)
      val once = System.nanoTime()
      val second = plan.smooth(surface, two).fold(e => fail(e.message), identity)
      val twice = System.nanoTime()
      assert(first.topology eq second.topology)
      if run >= 3 then
        preparation += prepared - start
        single += once - prepared
        double += twice - once
      run += 1
    val result = CoordinateSmoothing
      .prepare(topology)(SmoothingBoundary.Free)
      .smooth(surface, two)
      .fold(e => fail(e.message), identity)
    val output = result.positions.interleavedDoubles
    val before = SmoothingFixtures.measure(points, faces)
    val after = SmoothingFixtures.measure(output, faces)
    val areaDrift = math.abs(after.area / before.area - 1.0)
    val volumeDrift = math.abs(after.volume / before.volume - 1.0)
    var displacement = 0.0
    var i = 0
    while i < points.length do
      val dx = output(i) - points(i)
      val dy = output(i + 1) - points(i + 1)
      val dz = output(i + 2) - points(i + 2)
      displacement = math.max(displacement, math.sqrt(dx * dx + dy * dy + dz * dz))
      i += 3
    val box = surface.boundingBox.get
    val diagonal =
      math.sqrt(box.minimum.zip(box.maximum).map((a, b) => (a - b) * (a - b)).sum)
    assert(areaDrift <= 0.005, s"area drift $areaDrift")
    assert(volumeDrift <= 0.005, s"volume drift $volumeDrift")
    assert(
      displacement <= 0.001 * diagonal,
      s"displacement $displacement; diagonal $diagonal"
    )
    assertEquals(before.degenerate, 0)
    assertEquals(after.degenerate, 0)
    println(
      s"SMOOTHING_SCALE vertices=${topology.vertices.size} faces=${faces.size} areaBefore=${before.area} areaAfter=${after.area} volumeBefore=${before.volume} volumeAfter=${after.volume} maxDisplacement=$displacement diagonal=$diagonal degenerate=${after.degenerate} prepareMs=${preparation / 5e6} oneIterationRunMs=${single / 5e6} twoIterationRunMs=${double / 5e6}"
    )
