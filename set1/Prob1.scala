package Prob1

object Prob1 {

  def main(args: Array[String]): Unit = {
    var hexstr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    var out = hexTo64(hexstr)
    println(out)
  }

  var translate = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789:+/"
  def hexTo64(in: String): String = {
    var bin = new StringBuilder
    bin.append(BigInt(in, 16).toString(2))
    var base64 = new StringBuilder
    while (bin.length != in.length * 4) {
      bin.insert(0, "0")
    }
    while (bin.length % 24 != 0) {
      bin.append("0")
    }
    for (i <- 0 until (bin.length / 6.0).toInt) {
      var char = Integer.parseInt((bin slice(i*6, i*6+6)).mkString, 2)
      base64.append(translate(char))
    }
    while (base64.length % 4 != 0) {
      base64.append("=")
    }
    base64.toString()
  }
}
