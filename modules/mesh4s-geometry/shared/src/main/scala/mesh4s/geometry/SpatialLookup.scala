package mesh4s.geometry

import locus4s.Index
import mesh4s.Triangle
import mesh4s.TriangleTopology
import spatial4s.D3
import spatial4s.Frame
import spatial4s.Point

/** Exact branch-and-bound search over an immutable, domain-owned point field. Equal
  * distances select the lowest vertex ordinal. No neighborhood cutoff is used.
  */
final class NearestVertexIndex[S, F <: Frame[D3]](val positions: PointField[S, D3, F]):
  val frame: F = positions.frame
  private val tree = LookupTree.build(positions.space.size): i =>
    LookupBox.point(
      positions.packedValue(3 * i),
      positions.packedValue(3 * i + 1),
      positions.packedValue(3 * i + 2)
    )

  def nearest(query: Point[frame.type, D3]): Option[Index[S]] =
    val x = query.coordinate(0).get
    val y = query.coordinate(1).get
    val z = query.coordinate(2).get
    var best = -1
    var distance = Double.PositiveInfinity
    var overflowDistance = Double.PositiveInfinity
    tree.foreachNear(x, y, z)(() => distance): i =>
      val px = positions.packedValue(3 * i)
      val py = positions.packedValue(3 * i + 1)
      val pz = positions.packedValue(3 * i + 2)
      val d = LookupBox.norm(px - x, py - y, pz - z)
      // Finite inputs can still overflow subtraction or the final norm. Quarter
      // scaling distinguishes these distances without disturbing subnormal inputs.
      val scaled = if d.isInfinite then
        LookupBox.norm(px / 4 - x / 4, py / 4 - y / 4, pz / 4 - z / 4)
      else 0.0
      if best < 0 || d < distance || (d == distance && (scaled < overflowDistance || (scaled == overflowDistance && i < best)))
      then
        best = i
        distance = d
        overflowDistance = scaled
    if best < 0 then None else Some(positions.space.indexAtValidatedOrdinal(best))

/** A hit retains the face owner's index and weights in its admitted corner order. */
final case class RadialFaceHit[S](face: Index[S], weights: Triangle[Double])

/** Positive rays from the origin. Among admitted hits, select the largest minimum
  * barycentric weight, breaking ties by face ordinal. Boundaries admit weights down to
  * -1e-6, then clamp and normalize. Determinants below 1e-15 for a max-component-normalized
  * ray are refused. These are numerical tolerances, not a registration policy.
  */
final class RadialFaceIndex[T <: TriangleTopology, F <: Frame[D3]] private (
    val surface: SurfaceRealization[T, D3, F]
):
  val frame: F = surface.frame
  private val points = surface.positions
  private val faces = surface.topology.packedFaces
  private val tree = LookupTree.build(surface.topology.faces.size): face =>
    val corners = Array.tabulate(3)(k =>
      val i = faces(3 * face + k) * 3
      LookupBox.point(
        points.packedValue(i),
        points.packedValue(i + 1),
        points.packedValue(i + 2)
      )
    )
    // All weights >= -epsilon imply each coordinate lies within 2*epsilon
    // times the axis extent of the triangle. Include that envelope in the tree.
    corners.reduce(_.union(_)).expanded(2e-6)

  def locate(query: Point[frame.type, D3]): Option[RadialFaceHit[surface.topology.Face]] =
    val x = query.coordinate(0).get
    val y = query.coordinate(1).get
    val z = query.coordinate(2).get
    val magnitude = math.max(math.abs(x), math.max(math.abs(y), math.abs(z)))
    if magnitude == 0.0 then None
    else locateDirection(x / magnitude, y / magnitude, z / magnitude)

  private def locateDirection(
      x: Double,
      y: Double,
      z: Double
  ): Option[RadialFaceHit[surface.topology.Face]] =
    var best = -1
    var margin = Double.NegativeInfinity
    var weights = Triangle(0.0, 0.0, 0.0)
    tree.foreachRay(x, y, z): face =>
      hit(face, x, y, z).foreach: w =>
        val m = math.min(w.first, math.min(w.second, w.third))
        if m >= -1e-6 && (m > margin || (m == margin && (best < 0 || face < best))) then
          best = face
          margin = m
          weights = w
    if best < 0 then None
    else
      val a = math.max(0.0, weights.first)
      val b = math.max(0.0, weights.second)
      val c = math.max(0.0, weights.third)
      val total = a + b + c
      Some(
        RadialFaceHit(
          surface.topology.faces.indexAtValidatedOrdinal(best),
          Triangle(a / total, b / total, c / total)
        )
      )

  private def hit(face: Int, qx: Double, qy: Double, qz: Double): Option[Triangle[Double]] =
    val a = faces(3 * face) * 3
    val b = faces(3 * face + 1) * 3
    val c = faces(3 * face + 2) * 3
    val ax = points.packedValue(a)
    val ay = points.packedValue(a + 1)
    val az = points.packedValue(a + 2)
    val e1x = points.packedValue(b) - ax
    val e1y = points.packedValue(b + 1) - ay
    val e1z = points.packedValue(b + 2) - az
    val e2x = points.packedValue(c) - ax
    val e2y = points.packedValue(c + 1) - ay
    val e2z = points.packedValue(c + 2) - az
    val px = qy * e2z - qz * e2y
    val py = qz * e2x - qx * e2z
    val pz = qx * e2y - qy * e2x
    val det = e1x * px + e1y * py + e1z * pz
    if !det.isFinite || math.abs(det) < 1e-15 then None
    else
      val inv = 1.0 / det
      val u = (-ax * px - ay * py - az * pz) * inv
      val tx = -ay * e1z + az * e1y
      val ty = -az * e1x + ax * e1z
      val tz = -ax * e1y + ay * e1x
      val v = (qx * tx + qy * ty + qz * tz) * inv
      val t = (e2x * tx + e2y * ty + e2z * tz) * inv
      if t.isFinite && t > 0.0 && u.isFinite && v.isFinite then
        Some(Triangle(1.0 - u - v, u, v))
      else None

object RadialFaceIndex:
  /** Geometric admission never drops, repairs, or reorders a face. */
  def create[T <: TriangleTopology, F <: Frame[D3]](
      surface: SurfaceRealization[T, D3, F],
      tolerance: NondegeneracyTolerance
  ): Either[NondegeneracyReport, RadialFaceIndex[T, F]] =
    surface.requireNondegenerate(tolerance).map(_ => new RadialFaceIndex(surface))

private final case class LookupBox(lo: Array[Double], hi: Array[Double]):
  def union(that: LookupBox): LookupBox =
    LookupBox(
      Array.tabulate(3)(k => math.min(lo(k), that.lo(k))),
      Array.tabulate(3)(k => math.max(hi(k), that.hi(k)))
    )

  def expanded(relative: Double): LookupBox =
    val padding = Array.tabulate(3)(k =>
      relative * (hi(k) - lo(k)) + 1e-12 * math.max(
        1.0,
        math.max(math.abs(lo(k)), math.abs(hi(k)))
      )
    )
    LookupBox(
      Array.tabulate(3)(k => lo(k) - padding(k)),
      Array.tabulate(3)(k => hi(k) + padding(k))
    )

  def distance(x: Double, y: Double, z: Double): Double =
    def gap(q: Double, k: Int): Double = math.max(0.0, math.max(lo(k) - q, q - hi(k)))
    LookupBox.norm(gap(x, 0), gap(y, 1), gap(z, 2))

  def intersectsRay(x: Double, y: Double, z: Double): Boolean =
    var near = 0.0
    var far = Double.PositiveInfinity
    var axis = 0
    var possible = true
    while axis < 3 && possible do
      val direction = if axis == 0 then x else if axis == 1 then y else z
      if direction == 0.0 then possible = lo(axis) <= 0.0 && hi(axis) >= 0.0
      else
        val a = lo(axis) / direction
        val b = hi(axis) / direction
        near = math.max(near, math.min(a, b))
        far = math.min(far, math.max(a, b))
        possible = near <= far
      axis += 1
    possible && far > 0.0

private object LookupBox:
  def point(x: Double, y: Double, z: Double): LookupBox =
    LookupBox(Array(x, y, z), Array(x, y, z))
  def norm(x: Double, y: Double, z: Double): Double = math.hypot(math.hypot(x, y), z)

/** Private construction-time hierarchy; public APIs keep topology/domain owners. */
private enum LookupTree:
  case Empty
  case Leaf(box: LookupBox, items: Array[Int])
  case Branch(box: LookupBox, left: LookupTree, right: LookupTree)

  private def lowerBound(x: Double, y: Double, z: Double): Double = this match
    case Empty             => Double.PositiveInfinity
    case Leaf(box, _)      => box.distance(x, y, z)
    case Branch(box, _, _) => box.distance(x, y, z)

  def foreachNear(x: Double, y: Double, z: Double)(
      limit: () => Double
  )(visit: Int => Unit): Unit =
    if lowerBound(x, y, z) <= limit() then
      this match
        case Empty                  => ()
        case Leaf(_, items)         => items.foreach(visit)
        case Branch(_, left, right) =>
          val (first, second) =
            if left.lowerBound(x, y, z) <= right.lowerBound(x, y, z) then (left, right)
            else (right, left)
          first.foreachNear(x, y, z)(limit)(visit)
          second.foreachNear(x, y, z)(limit)(visit)

  def foreachRay(x: Double, y: Double, z: Double)(visit: Int => Unit): Unit = this match
    case Empty                    => ()
    case Leaf(box, items)         => if box.intersectsRay(x, y, z) then items.foreach(visit)
    case Branch(box, left, right) =>
      if box.intersectsRay(x, y, z) then
        left.foreachRay(x, y, z)(visit)
        right.foreachRay(x, y, z)(visit)

private object LookupTree:
  def build(size: Int)(bounds: Int => LookupBox): LookupTree =
    val boxes = Array.tabulate(size)(bounds)
    def loop(items: Array[Int]): LookupTree =
      if items.isEmpty then LookupTree.Empty
      else
        val box = items.iterator.map(boxes(_)).reduce(_.union(_))
        if items.length <= 8 then LookupTree.Leaf(box, items)
        else
          val axis = (0 until 3).maxBy(k => box.hi(k) - box.lo(k))
          val sorted = items.sortBy(i => boxes(i).lo(axis) * 0.5 + boxes(i).hi(axis) * 0.5)
          val middle = sorted.length / 2
          LookupTree.Branch(box, loop(sorted.take(middle)), loop(sorted.drop(middle)))
    loop(Array.tabulate(size)(identity))
