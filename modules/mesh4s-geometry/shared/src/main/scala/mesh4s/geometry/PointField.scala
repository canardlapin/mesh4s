package mesh4s.geometry

import locus4s.DomainAlignment
import locus4s.FiniteDomain
import locus4s.Index
import spatial4s.D2
import spatial4s.D3
import spatial4s.Dim
import spatial4s.Dimension
import spatial4s.Frame
import spatial4s.Point

enum PointStoragePrecision derives CanEqual:
  case Float32
  case Float64

enum PointFieldError derives CanEqual:
  case CoordinateCountMismatch(expected: Int, actual: Int)
  case NonFiniteCoordinate(vertex: Int, axis: Int, value: Double)

  def message: String =
    this match
      case CoordinateCountMismatch(expected, actual) =>
        s"point field requires $expected coordinates, found $actual"
      case NonFiniteCoordinate(vertex, axis, value) =>
        s"vertex $vertex axis $axis has non-finite coordinate $value"

/** Primitive callback for allocation-free traversal of packed D2 points. */
trait D2PointConsumer[S]:
  def apply(index: Index[S], x: Double, y: Double): Unit

/** Primitive callback for allocation-free traversal of packed D3 points. */
trait D3PointConsumer[S]:
  def apply(index: Index[S], x: Double, y: Double, z: Double): Unit

/** Frame-owned packed coordinates over one exact locus4s domain. */
sealed abstract class PointField[
    S,
    D <: Dim,
    F <: Frame[D]
] private[geometry] (
    val space: FiniteDomain[S],
    val frame: F
)(using val dimension: Dimension[D]):
  def precision: PointStoragePrecision

  private[geometry] def valueAtOffset(offset: Int): Double

  final def coordinate(index: Index[S], axis: Int): Option[Double] =
    if axis >= 0 && axis < dimension.rank then
      Some(valueAtOffset(index.ordinal * dimension.rank + axis))
    else None

  final def point(index: Index[S]): Point[frame.type, D] =
    Point
      .fromVector(
        frame,
        Vector.tabulate(dimension.rank)(axis =>
          valueAtOffset(index.ordinal * dimension.rank + axis)
        )
      )
      .fold(
        error =>
          throw new IllegalStateException(
            s"validated point field produced ${error.message}"
          ),
        identity
      )

  final def foreachD2(
      f: D2PointConsumer[S]
  )(using D =:= D2): Unit =
    var ordinal = 0
    while ordinal < space.size do
      val offset = ordinal * 2
      f(
        space.indexAtValidatedOrdinal(ordinal),
        valueAtOffset(offset),
        valueAtOffset(offset + 1)
      )
      ordinal += 1

  final def foreachD3(
      f: D3PointConsumer[S]
  )(using D =:= D3): Unit =
    var ordinal = 0
    while ordinal < space.size do
      val offset = ordinal * 3
      f(
        space.indexAtValidatedOrdinal(ordinal),
        valueAtOffset(offset),
        valueAtOffset(offset + 1),
        valueAtOffset(offset + 2)
      )
      ordinal += 1

  final def interleavedDoubles: Array[Double] =
    Array.tabulate(space.size * dimension.rank)(valueAtOffset)

  def interleavedFloats: Array[Float]

  final def rebind[T](
      alignment: DomainAlignment[S, T]
  ): PointField[T, D, F] =
    PointField.view(alignment.right, frame, this)

private final class DoublePointField[S, D <: Dim, F <: Frame[D]](
    space: FiniteDomain[S],
    frame: F,
    private val coordinates: Array[Double]
)(using Dimension[D])
    extends PointField[S, D, F](space, frame):
  val precision: PointStoragePrecision =
    PointStoragePrecision.Float64

  private[geometry] def valueAtOffset(offset: Int): Double =
    coordinates(offset)

  def interleavedFloats: Array[Float] =
    coordinates.map(_.toFloat)

private final class FloatPointField[S, D <: Dim, F <: Frame[D]](
    space: FiniteDomain[S],
    frame: F,
    private val coordinates: Array[Float]
)(using Dimension[D])
    extends PointField[S, D, F](space, frame):
  val precision: PointStoragePrecision =
    PointStoragePrecision.Float32

  private[geometry] def valueAtOffset(offset: Int): Double =
    coordinates(offset).toDouble

  def interleavedFloats: Array[Float] =
    coordinates.clone()

private final class ReboundPointField[
    S,
    D <: Dim,
    F <: Frame[D]
](
    space: FiniteDomain[S],
    frame: F,
    source: PointField[?, D, F]
)(using Dimension[D])
    extends PointField[S, D, F](space, frame):
  def precision: PointStoragePrecision =
    source.precision

  private[geometry] def valueAtOffset(offset: Int): Double =
    source.valueAtOffset(offset)

  def interleavedFloats: Array[Float] =
    source.interleavedFloats

object PointField:
  def fromInterleavedDoubles[S, D <: Dim](
      space: FiniteDomain[S],
      frame: Frame[D],
      coordinates: Array[Double]
  )(using
      dimension: Dimension[D]
  ): Either[PointFieldError, PointField[S, D, Frame[D]]] =
    validate(space.size, dimension.rank, coordinates.iterator.map(identity)).map: _ =>
      new DoublePointField[S, D, Frame[D]](
        space,
        frame,
        coordinates.clone()
      )

  def fromInterleavedFloats[S, D <: Dim](
      space: FiniteDomain[S],
      frame: Frame[D],
      coordinates: Array[Float]
  )(using
      dimension: Dimension[D]
  ): Either[PointFieldError, PointField[S, D, Frame[D]]] =
    validate(
      space.size,
      dimension.rank,
      coordinates.iterator.map(_.toDouble)
    ).map: _ =>
      new FloatPointField[S, D, Frame[D]](
        space,
        frame,
        coordinates.clone()
      )

  private def validate(
      size: Int,
      rank: Int,
      coordinates: Iterator[Double]
  ): Either[PointFieldError, Unit] =
    val values = coordinates.toVector
    val expected = size * rank
    if values.length != expected then
      Left(PointFieldError.CoordinateCountMismatch(expected, values.length))
    else
      values.zipWithIndex.find((value, _) => !value.isFinite) match
        case Some((value, offset)) =>
          Left(
            PointFieldError.NonFiniteCoordinate(
              offset / rank,
              offset % rank,
              value
            )
          )
        case None => Right(())

  private def view[S, D <: Dim, F <: Frame[D]](
      space: FiniteDomain[S],
      frame: F,
      source: PointField[?, D, F]
  )(using Dimension[D]): PointField[S, D, F] =
    new ReboundPointField(space, frame, source)
