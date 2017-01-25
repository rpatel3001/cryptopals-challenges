import java.util.Base64
import Util._

object Prob20 {
  def main(args: Array[String]): Unit = {
    val globalkey = keygen
    val base64 = io.Source.fromFile("data20.txt").getLines.toArray
    val ciphers = base64.map(c ⇒ AESCTRTransform(Base64.getDecoder.decode(c), globalkey, Array.fill[Byte](8)(0)))
    val minlen = ciphers.sortBy(_.size).head.size
    val ciph = ciphers.map(_.take(minlen))
    val xored = ciph.map(xor(_, ciph.head)).flatten
    val broke = breakXOR(xored, minlen)
    val ans = "I'm rated \"R\"...this is a warning, ya better void / PCuz I came back to attack others in spite- / Strike l"
    val out = broke._4.take(ans.size)
    if (out == ans) {
      println("Prob 20: Success")
    } else {
      println("Prob 20: Fail")
    }
  }
}