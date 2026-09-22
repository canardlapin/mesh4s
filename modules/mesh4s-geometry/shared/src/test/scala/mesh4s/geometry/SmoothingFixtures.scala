package mesh4s.geometry

import mesh4s.Triangle

/** Synthetic scale court: corrugated sphere, no participant/anatomical data. */
private[geometry] object SmoothingFixtures:
  def sphere(rings: Int, columns: Int): (Array[Double], Vector[Triangle[Int]]) =
    val points = new Array[Double]((rings * columns + 2) * 3)
    points(2) = 100.0
    points(points.length - 1) = -100.0
    def vertex(row: Int, column: Int): Int = 1 + row * columns + column % columns
    var row = 0
    while row < rings do
      val theta = math.Pi * (row + 1) / (rings + 1)
      var column = 0
      while column < columns do
        val phi = 2.0 * math.Pi * column / columns
        val radius = 100.0 * (1.0 + 0.03 * math.sin(12 * theta) * math.sin(10 * phi) * math
          .pow(math.sin(theta), 2))
        val offset = vertex(row, column) * 3
        points(offset) = radius * math.sin(theta) * math.cos(phi)
        points(offset + 1) = radius * math.sin(theta) * math.sin(phi)
        points(offset + 2) = radius * math.cos(theta)
        column += 1
      row += 1
    val faces = Vector.newBuilder[Triangle[Int]]
    var column = 0
    while column < columns do
      faces += Triangle(0, vertex(0, column), vertex(0, column + 1))
      row = 0
      while row < rings - 1 do
        val a = vertex(row, column)
        val b = vertex(row + 1, column)
        val c = vertex(row + 1, column + 1)
        val d = vertex(row, column + 1)
        faces += Triangle(a, b, c)
        faces += Triangle(a, c, d)
        row += 1
      faces += Triangle(
        vertex(rings - 1, column),
        points.length / 3 - 1,
        vertex(rings - 1, column + 1)
      )
      column += 1
    (points, faces.result())

  final case class Measures(area: Double, volume: Double, degenerate: Int)

  // Independent cross-product/determinant measurement, not production geometry.
  def measure(p: Array[Double], faces: Vector[Triangle[Int]]): Measures =
    var area = 0.0
    var volume = 0.0
    var degenerate = 0
    faces.foreach: t =>
      val a = t.first * 3
      val b = t.second * 3
      val c = t.third * 3
      val ux = p(b) - p(a)
      val uy = p(b + 1) - p(a + 1)
      val uz = p(b + 2) - p(a + 2)
      val vx = p(c) - p(a)
      val vy = p(c + 1) - p(a + 1)
      val vz = p(c + 2) - p(a + 2)
      val nx = uy * vz - uz * vy
      val ny = uz * vx - ux * vz
      val nz = ux * vy - uy * vx
      val faceArea = 0.5 * math.sqrt(nx * nx + ny * ny + nz * nz)
      area += faceArea
      volume += (p(a) * nx + p(a + 1) * ny + p(a + 2) * nz) / 6.0
      val wx = p(c) - p(b)
      val wy = p(c + 1) - p(b + 1)
      val wz = p(c + 2) - p(b + 2)
      val scale = math.max(
        ux * ux + uy * uy + uz * uz,
        math.max(vx * vx + vy * vy + vz * vz, wx * wx + wy * wy + wz * wz)
      )
      if !faceArea.isFinite || faceArea <= 1e-12 * scale then degenerate += 1
    Measures(area, volume, degenerate)
