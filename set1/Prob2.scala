object HexXOR {
    def main(args: Array[String]): Unit = {
        var hex1 = "1c0111001f010100061a024b53535009181c"
        var hex2 = "686974207468652062756c6c277320657965"
        println(hexXOR(hex1, hex2))
    }

    def hexXOR(str1: String, str2: String): String = {
        var big1 = BigInt(str1, 16)
        var big2 = BigInt(str2, 16)
        (big1 ^ big2).toString(16)
    }
}
