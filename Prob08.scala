import Util._

object Prob08 {
  def main(args: Array[String]): Unit = {
    val lines = io.Source.fromFile("data8.txt").getLines
    var maxident = 0
    var ecb = ""
    for (line ← lines) {
      val ident = detectECBRepeats(line.getBytes)
      if (ident > maxident) {
        maxident = ident
        ecb = line
      }
    }
    if (ecb.slice(0, 10) == "d880619740") {
      println("Prob 08: Success")
    } else {
      println("Prob 08: Fail")
    }
  }
}