import java.util.Base64

object Prob1 {

  def main(args: Array[String]): Unit = {
    var hexstr = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"
    var out = Base64.getEncoder().encodeToString(toASCII(hexstr).getBytes)
    if(out == "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t") {
      println("Prob 1: Success")
    } else {
      println("Prob 1: Fail")
    }
  }

  def hexval(char: Char): Int = {
    if (char >= '0' && char <= '9') {
      char - '0'
    } else if (char >= 'a' && char <= 'f') {
      char - 'a' + 10
    } else if (char >= 'A' && char <= 'F') {
      char - 'A' + 10
    } else {
      throw new RuntimeException
    }
  }

    def toASCII(str: String): String = {
        var ascii = new StringBuilder
        for (i <- 0 until str.length / 2) {
            ascii.append(((hexval(str(i*2)) << 4) + hexval(str(i*2+1))).toChar)
        }
        ascii.toString
    }
}
