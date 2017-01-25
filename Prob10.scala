import java.util.Base64
import Util._

object Prob10 {
  def main(args: Array[String]) = {
    val base64 = io.Source.fromFile("data10.txt").mkString
    val bytes = Base64.getMimeDecoder().decode(base64)
    val iv = Array.fill[Byte](16)(0)
    val key = "YELLOW SUBMARINE".getBytes
    val decoded = decodeAESCBC(bytes, key, iv)
    val encoded = encodeAESCBC(decoded, key, iv)
    if (toASCII(encoded) == toASCII(bytes) && toASCII(decoded).slice(0, 33) == "I'm back and I'm ringin' the bell") {
      println("Prob 10: Success")
    } else {
      println("Prob 10: Fail")
    }
  }
}