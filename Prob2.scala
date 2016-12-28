object Prob2 {
    def main(args: Array[String]): Unit = {
        var hex1 = "1c0111001f010100061a024b53535009181c"
        var hex2 = "686974207468652062756c6c277320657965"
        var out = hexXOR(hex1, hex2)
        if(out == "746865206b696420646f6e277420706c6179") {
          println("Prob 2: Success")
        } else {
          println("Prob 2: Fail")
        }
    }

    def hexXOR(str1: String, str2: String): String = {
        var big1 = BigInt(str1, 16)
        var big2 = BigInt(str2, 16)
        (big1 ^ big2).toString(16)
    }
}
