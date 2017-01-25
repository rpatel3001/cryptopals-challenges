import scala.util.Random
import Util._

object Prob11 {
  def main(args: Array[String]): Unit = {
    val data = ("A" * 128).getBytes
    for (i ← 0 to 100) {
      val encr = randomEncrypt(data)
      if (getAESMode(encr._1) != encr._2) {
        println("Prob 11: Fail")
        return
      }
    }
    println("Prob 11: Success")
  }

  def randomEncrypt(data: Array[Byte]): (Array[Byte], String) = {
    val c1 = Random.nextInt(5) + 5
    val c2 = Random.nextInt(5) + 5
    val d = padPKCS7(Random.nextString(c1).getBytes ++ data ++ Random.nextString(c2).getBytes, 16)
    if (Random.nextInt(2) == 0) {
      (encodeAESECB(d, keygen), "ECB")
    } else {
      (encodeAESCBC(d, keygen, keygen), "CBC")
    }
  }
}