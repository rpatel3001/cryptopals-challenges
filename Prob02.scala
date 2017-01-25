import Util._

object Prob02 {
  def main(args: Array[String]): Unit = {
    val hex1 = BigInt("1c0111001f010100061a024b53535009181c", 16).toByteArray
    val hex2 = BigInt("686974207468652062756c6c277320657965", 16).toByteArray
    val out = xor(hex1, hex2)
    if (toHex(out) == "746865206b696420646f6e277420706c6179") {
      println("Prob 02: Success")
    } else {
      println("Prob 02: Fail")
    }
  }
}
