object Prob02 {
    def main(args: Array[String]): Unit = {
        var hex1 = BigInt("1c0111001f010100061a024b53535009181c", 16).toByteArray
        var hex2 = BigInt("686974207468652062756c6c277320657965", 16).toByteArray
        var out = xor(hex1, hex2)
        if(toHex(out) == "746865206b696420646f6e277420706c6179") {
          println("Prob 02: Success")
        } else {
          println("Prob 02: Fail")
        }
    }

    def toHex(str: Array[Byte]): String = {
        str.map(byteToHex).mkString
    }

    def byteToHex(b: Byte): String = {
        if (b < 0x10) {
            "0" + b.toHexString
        } else {
            b.toHexString
        }
    }

    def xor(val1: Array[Byte], val2: Array[Byte]): Array[Byte] = {
    	var ret = collection.mutable.ArrayBuffer[Byte]()
    	for(i <- 0 until val1.length) {
    		ret.append((val1(i) ^ val2(i)).toByte)
    	}
    	ret.toArray
	}
}
