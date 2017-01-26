import util.Random
import Util._

object Prob24 {
  def main(args: Array[String]): Unit = {
    val key = Random.nextInt & 0xFF
    val pre = Random.alphanumeric.take(Random.nextInt(16)).mkString
    val str = "A" * 14
    val plain = (pre + str).getBytes
    val ciph = prngTransform(key, plain)

    val token = generateResetToken
    val invalid = "password".getBytes

    if (plain.mkString != prngTransform(key, ciph).mkString) {
      println("Prob 24: Fail")
    } else if (key != recoverKey(str.getBytes, ciph)) {
      println("Prob 24: Fail")
    } else if(!checkToken(token) || checkToken(invalid)) {
      println("Prob 24: Fail")
    } else {
      println("Prob 24: Success")
    }
  }

  def prngTransform(seed: Int, data: Array[Byte]): Array[Byte] = {
    val rng = new MT19937(seed)
    data.map(c => (c ^ (rng.rand & 0xFF)).toByte)
  }

  def recoverKey(plain: Array[Byte], data: Array[Byte]): Int = {
    (0 to 255).map(c => (c, prngTransform(c, data))).filter({case (a,b) => b.takeRight(plain.size).mkString == plain.mkString})(0)._1
  }

  def generateResetToken(): Array[Byte] = {
    val rng = new MT19937(System.currentTimeMillis.toInt)
    toHex((1 to 8).map(c => rng.rand.toByte).toArray).getBytes
  }

  def checkToken(tok: Array[Byte]): Boolean = {
    (-10000 to 10000).map(c => {
      val rng = new MT19937(System.currentTimeMillis.toInt - c)
      toHex((1 to 8).map(c => rng.rand.toByte).toArray).getBytes.mkString
    }).filter(_ == tok.mkString).size != 0
  }
}