import java.util.Base64
import Util._

object Prob18 {
  def main(args: Array[String]): Unit = {
    val ciph = Base64.getDecoder.decode("L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ==")
    val ans = "Yo, VIP Let's kick it Ice, Ice, baby Ice, Ice, baby ".getBytes
    val out = AESCTRTransform(ciph, "YELLOW SUBMARINE".getBytes, Array.fill[Byte](8)(0))
    if (out.mkString == ans.mkString) {
      println("Prob 18: Success")
    } else {
      println("Prob 18: Fail")
    }
  }
}