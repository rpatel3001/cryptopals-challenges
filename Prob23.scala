import Prob21._

object Prob23 {
  def main(args: Array[String]): Unit = {
    val rng = new MT19937(13416)
    val clone = new MT19937(89824)
    val mt = (1 to 624).map(c => untemper(rng.rand))
    clone.mt = mt.toArray
    if(rng.mt.zip(clone.mt).count({case (a,b) => a != b}) == 0) {
        println("Prob 23: Success")
    } else {
        println("Prob 23: Fail")
    }
  }

  val U = 11
  val S = 7
  val B = 0x9D2C5680
  val T = 15
  val C = 0xEFC60000
  val L = 18
  val D = 0xFFFFFFFF
  def untemper(x: Int): Int = {
    var y = x ^ (x >>> L)

    var mask = (1 << T) - 1
    y = y ^ (C >>> T & mask & (y & mask)) << T
    y = y ^ (C >>> (2*T) & mask & (y >>> T & mask)) << (2*T)
    y = y ^ (C >>> (3*T) & mask & (y >>> (2*T) & mask)) << (3*T)

    mask = (1 << S) - 1
    y = y ^ (B >>> S & mask & (y & mask)) << S
    y = y ^ (B >>> (2*S) & mask & (y >>> S & mask)) << (2*S)
    y = y ^ (B >>> (3*S) & mask & (y >>> (2*S) & mask)) << (3*S)
    y = y ^ (B >>> (4*S) & mask & (y >>> (3*S) & mask)) << (4*S)

    y = y ^ (y >>> U) & 0x1ffc00
    y ^ (y & 0x1fffff) >>> U
  }
}