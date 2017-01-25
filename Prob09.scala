import Util._

object Prob09 {
  def main(args: Array[String]) = {
    val orig = "YELLOW SUBMARINE".getBytes
    val ans = orig ++ Array.fill[Byte](4)(4)
    val out = padPKCS7(orig, 20)
    if (out.mkString == ans.mkString) {
      println("Prob 09: Success")
    } else {
      println("Prob 09: Fail")
    }
  }
}
