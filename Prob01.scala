import java.util.Base64

object Prob01 {
  def main(args: Array[String]): Unit = {
    var hexstr = BigInt("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d", 16).toByteArray
    var out = Base64.getEncoder().encodeToString(hexstr)
    if(out == "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t") {
      println("Prob 01: Success")
    } else {
      println("Prob 01: Fail")
    }
  }
}
