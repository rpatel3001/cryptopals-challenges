import Prob03._

object Prob04 {
  def main(args: Array[String]): Unit = {
    var min = (' '.toByte, 1000000.0)
    var text = ""
    for (l ← io.Source.fromFile("data4.txt").getLines()) {
      var line = BigInt(l, 16).toByteArray
      var tmp = decrypt(line)
      if (tmp._2 < min._2) {
        min = tmp
        text = toASCII(keyXOR(line, Array(tmp._1)))
      }
    }
    //print("Prob 4: " + min + " " + text)
    if (text == "Now that the party is jumping\n") {
      println("Prob 04: Success")
    } else {
      println("Prob 04: Fail")
    }
  }
}