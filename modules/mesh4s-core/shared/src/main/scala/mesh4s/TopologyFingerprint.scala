package mesh4s

/** Version of the canonical byte stream hashed for connectivity fingerprints. */
enum TopologyFingerprintScheme(val version: Int) derives CanEqual:
  case OrderedTrianglesV1 extends TopologyFingerprintScheme(1)

object TopologyFingerprintScheme:
  def fromVersion(version: Int): Option[TopologyFingerprintScheme] =
    TopologyFingerprintScheme.values.find(_.version == version)

/** Structural SHA-256 evidence for an ordered triangle table.
  *
  * A fingerprint does not carry owner identity and never authorizes index transport.
  */
final class TopologyFingerprint private (val value: String):
  override def equals(other: Any): Boolean =
    other match
      case that: TopologyFingerprint => value == that.value
      case _                         => false

  override def hashCode(): Int =
    value.hashCode()

  override def toString: String =
    s"TopologyFingerprint($value)"

object TopologyFingerprint:
  enum ParseError derives CanEqual:
    case InvalidSha256(value: String)

    def message: String =
      this match
        case InvalidSha256(value) =>
          s"topology fingerprint must be 64 hexadecimal characters, found '$value'"

  def parse(value: String): Either[ParseError, TopologyFingerprint] =
    val normalized = value.trim.toLowerCase
    if normalized.length == 64 && normalized.forall(isHexDigit) then
      Right(new TopologyFingerprint(normalized))
    else Left(ParseError.InvalidSha256(value))

  def compute(
      vertexCount: Int,
      faceVertexOrdinals: IterableOnce[Int],
      scheme: TopologyFingerprintScheme = TopologyFingerprintScheme.OrderedTrianglesV1
  ): TopologyFingerprint =
    val digest = new Sha256
    digest.updateInt(scheme.version)
    digest.updateInt(vertexCount)
    faceVertexOrdinals.iterator.foreach(digest.updateInt)
    new TopologyFingerprint(digest.finishHex())

  private def isHexDigit(char: Char): Boolean =
    (char >= '0' && char <= '9') ||
      (char >= 'a' && char <= 'f')

/** Small shared SHA-256 implementation used to keep JVM and Scala.js bytes identical. */
private final class Sha256:
  private val state =
    Array(
      0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab,
      0x5be0cd19
    )
  private val block = Array.ofDim[Byte](64)
  private val words = Array.ofDim[Int](64)
  private var blockLength = 0
  private var messageLength = 0L

  def updateInt(value: Int): Unit =
    updateByte((value >>> 24).toByte)
    updateByte((value >>> 16).toByte)
    updateByte((value >>> 8).toByte)
    updateByte(value.toByte)

  def finishHex(): String =
    val bitLength = messageLength * 8L
    appendPaddingByte(0x80.toByte)
    while blockLength != 56 do appendPaddingByte(0.toByte)
    var shift = 56
    while shift >= 0 do
      appendPaddingByte((bitLength >>> shift).toByte)
      shift -= 8

    val output = Array.ofDim[Byte](32)
    var stateIndex = 0
    while stateIndex < state.length do
      val value = state(stateIndex)
      val offset = stateIndex * 4
      output(offset) = (value >>> 24).toByte
      output(offset + 1) = (value >>> 16).toByte
      output(offset + 2) = (value >>> 8).toByte
      output(offset + 3) = value.toByte
      stateIndex += 1
    hex(output)

  private def updateByte(value: Byte): Unit =
    block(blockLength) = value
    blockLength += 1
    messageLength += 1L
    if blockLength == 64 then processBlock()

  private def appendPaddingByte(value: Byte): Unit =
    block(blockLength) = value
    blockLength += 1
    if blockLength == 64 then processBlock()

  private def processBlock(): Unit =
    var index = 0
    while index < 16 do
      val offset = index * 4
      words(index) = ((block(offset) & 0xff) << 24) |
        ((block(offset + 1) & 0xff) << 16) |
        ((block(offset + 2) & 0xff) << 8) |
        (block(offset + 3) & 0xff)
      index += 1
    while index < 64 do
      val first = smallSigma1(words(index - 2))
      val second = words(index - 7)
      val third = smallSigma0(words(index - 15))
      val fourth = words(index - 16)
      words(index) = first + second + third + fourth
      index += 1

    var a = state(0)
    var b = state(1)
    var c = state(2)
    var d = state(3)
    var e = state(4)
    var f = state(5)
    var g = state(6)
    var h = state(7)
    index = 0
    while index < 64 do
      val first = h + bigSigma1(e) + choose(e, f, g) + Sha256.constants(index) +
        words(index)
      val second = bigSigma0(a) + majority(a, b, c)
      h = g
      g = f
      f = e
      e = d + first
      d = c
      c = b
      b = a
      a = first + second
      index += 1

    state(0) += a
    state(1) += b
    state(2) += c
    state(3) += d
    state(4) += e
    state(5) += f
    state(6) += g
    state(7) += h
    blockLength = 0

  private def rotateRight(value: Int, distance: Int): Int =
    (value >>> distance) | (value << (32 - distance))

  private def choose(x: Int, y: Int, z: Int): Int =
    (x & y) ^ (~x & z)

  private def majority(x: Int, y: Int, z: Int): Int =
    (x & y) ^ (x & z) ^ (y & z)

  private def bigSigma0(value: Int): Int =
    rotateRight(value, 2) ^ rotateRight(value, 13) ^ rotateRight(value, 22)

  private def bigSigma1(value: Int): Int =
    rotateRight(value, 6) ^ rotateRight(value, 11) ^ rotateRight(value, 25)

  private def smallSigma0(value: Int): Int =
    rotateRight(value, 7) ^ rotateRight(value, 18) ^ (value >>> 3)

  private def smallSigma1(value: Int): Int =
    rotateRight(value, 17) ^ rotateRight(value, 19) ^ (value >>> 10)

  private def hex(bytes: Array[Byte]): String =
    val digits = "0123456789abcdef"
    val output = Array.ofDim[Char](bytes.length * 2)
    var index = 0
    while index < bytes.length do
      val value = bytes(index) & 0xff
      output(index * 2) = digits.charAt(value >>> 4)
      output(index * 2 + 1) = digits.charAt(value & 0x0f)
      index += 1
    new String(output)

private object Sha256:
  val constants: Array[Int] =
    Array(
      0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
      0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe,
      0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f,
      0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
      0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
      0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
      0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116,
      0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
      0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
      0xc67178f2
    )
