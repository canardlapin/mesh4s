package mesh4s.geometry

import scala.compiletime.testing.typeCheckErrors

final class SmoothingOwnershipSuite extends munit.FunSuite:
  test("a constraint from another topology owner is rejected statically"):
    val errors = typeCheckErrors("""
      import mesh4s.*
      import mesh4s.geometry.*
      val first = TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))).toOption.get
      val second = TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))).toOption.get
      val foreign = second.vertices.index(0).toOption.get
      CoordinateSmoothing.prepare(first)(SmoothingBoundary.Free, Iterator(foreign))
    """)
    assert(errors.nonEmpty)

  test("a realization from another topology owner is rejected statically"):
    val errors = typeCheckErrors("""
      import mesh4s.*
      import mesh4s.geometry.*
      import spatial4s.{D3, Frame}
      val first = TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))).toOption.get
      val second = TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))).toOption.get
      val frame = Frame.named[D3]("display").toOption.get
      val surface = SurfaceRealization.fromInterleavedDoubles(second, frame, Array.fill(9)(0.0)).toOption.get
      CoordinateSmoothing.prepare(first)(SmoothingBoundary.Free)
        .smooth(surface, SmoothingParameters.create(1, 0.1).toOption.get)
    """)
    assert(errors.nonEmpty)

  test("the public path preserves singleton topology and coordinate frame types"):
    val errors = typeCheckErrors("""
      import mesh4s.*
      import mesh4s.geometry.*
      import spatial4s.{D3, Frame}
      val topology = TriangleTopology.fromOrdinalFaces(3, Vector(Triangle(0, 1, 2))).toOption.get
      val frame = Frame.named[D3]("display").toOption.get
      val points = PointField.fromInterleavedDoubles(topology.vertices, frame, Array.fill(9)(0.0)).toOption.get
      val surface = SurfaceRealization.fromPointField(topology, points).toOption.get
      val smoothed: SurfaceRealization[topology.type, D3, Frame[D3]] =
        CoordinateSmoothing.prepare(topology)(SmoothingBoundary.Free)
          .smooth(surface, SmoothingParameters.create(1, 0.1).toOption.get).toOption.get
      smoothed.position(topology.vertices.index(0).toOption.get)
    """)
    assertEquals(errors, Nil)

  test("generic consumers can prepare directly from their realization"):
    val errors = typeCheckErrors("""
      import mesh4s.*
      import mesh4s.geometry.*
      import spatial4s.{Dim, Frame}
      def smooth[T <: TriangleTopology, D <: Dim, F <: Frame[D]](
          surface: SurfaceRealization[T, D, F], parameters: SmoothingParameters
      ): Either[SmoothingError, SurfaceRealization[T, D, F]] =
        CoordinateSmoothing.prepare(surface.topology)(SmoothingBoundary.Fixed)
          .smooth(surface, parameters)
    """)
    assertEquals(errors, Nil)
