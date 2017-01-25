import java.util.Base64
import Util._

object Prob07 {
  def main(args: Array[String]): Unit = {
    val base64 = io.Source.fromFile("data7.txt").mkString
    val bytes = Base64.getMimeDecoder().decode(base64)
    if (toASCII(decodeAESECB(bytes, "YELLOW SUBMARINE".getBytes)).slice(0, 33) == "I'm back and I'm ringin' the bell") {
      println("Prob 07: Success")
    } else {
      println("Prob 07: Fail")
    }
  }
}