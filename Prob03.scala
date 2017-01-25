import Util._

object Prob03 {
  def main(args: Array[String]): Unit = {
    val str = BigInt("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736", 16).toByteArray
    val res = decryptVignere(str)
    //println("Prob 3: " + res + " " + toASCII(str).map(c ⇒ (c ^ res._1).toChar))
    if (toASCII(str).map(c ⇒ (c ^ res._1).toChar) == "Cooking MC's like a pound of bacon") {
      println("Prob 03: Success")
    } else {
      println("Prob 03: Fail")
    }
  }
}