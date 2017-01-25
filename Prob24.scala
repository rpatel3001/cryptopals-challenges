import util.Random
import Util._

object Prob24 {
  def main(args: Array[String]): Unit = {
    val key = Random.nextInt & 0xFF
    val str = "test string".getBytes
    if (toASCII(str) == toASCII(prngTransform(key, prngTransform(key, str)))) {
      println("Prob 24: Success")
    } else {
      println("Prob 24: Fail")
    }
  }

  def prngTransform(seed: Int, data: Array[Byte]): Array[Byte] = {
    val rng = new MT19937(seed)
    data.map(c => (c ^ (rng.rand & 0xFF)).toByte)
  }
}