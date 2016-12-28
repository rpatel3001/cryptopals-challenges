import java.util.Base64

object Prob1 {

  def main(args: Array[String]): Unit = {
    var hexstr = BigInt("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d", 16)
    var out = Base64.getEncoder().encodeToString(hexstr.toByteArray)
    if(out == "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t") {
      println("Prob 1: Success")
    } else {
      println("Prob 1: Fail")
    }
  }
}
