import Prob2.xor

object Prob3 {
    def main(args: Array[String]): Unit = {
        var str = BigInt("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736", 16).toByteArray
        var res = decrypt(str)
        println("Prob 3: " + res + " " + toASCII(str).map(c => (c ^ res._1).toChar))
    }

    def decrypt(str: Array[Byte]): (Byte, Double) = {
        var results: collection.mutable.Map[Byte, Double] = collection.mutable.Map()
        for (char <- '0' to '9') {
            var tmp = str
            tmp = keyXOR(tmp, Array(char.toByte))
            //if(isASCII(tmp))
            results += char.toByte -> score(tmp)
        }
        for (char <- 'A' to 'Z') {
            var tmp = str
            tmp = keyXOR(tmp, Array(char.toByte))
            //if(isASCII(tmp))
            results += char.toByte -> score(tmp)
        }
        for (char <- 'a' to 'z') {
            var tmp = str
            tmp = keyXOR(tmp, Array(char.toByte))
            //if(isASCII(tmp))
            results += char.toByte -> score(tmp)
        }
        results.toSeq.sortBy(_._2) apply 0
    }

    def isASCII(str: String): Boolean = {
        for(char <- str) {
            if (!(char == '\n' || char == '\r' || char == '\t' || char >= 32 && char <= 126)) {
                return false
            }
        }
        return true
    }

    def toASCII(raw: Array[Byte]): String = {
    	raw.map(_.toChar).mkString
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

    def charToHex(char: Char): String = {
    	if (char < 0x10) {
    		"0" + char.toHexString
    	} else {
    		char.toHexString
    	}
    }

	def byteToHex(b: Byte): String = {
		charToHex(b.toChar)
	}

	def keyXOR(str: Array[Byte], key: Array[Byte]): Array[Byte] = {
		var len = str.length
		var k = Array.fill(len/key.length + 1)(key).flatten.slice(0, len)
		xor(str, k)
	}

    var frequency = Map(' ' -> .11504,
                        'a' -> .07227,
                        'b' -> .01320,
                        'c' -> .02462,
                        'd' -> .03764,
                        'e' -> .11241,
                        'f' -> .01972,
                        'g' -> .01783,
                        'h' -> .05393,
                        'i' -> .06165,
                        'j' -> .00135,
                        'k' -> .00683,
                        'l' -> .03562,
                        'm' -> .02129,
                        'n' -> .05973,
                        'o' -> .06643,
                        'p' -> .01707,
                        'q' -> .00084,
                        'r' -> .05298,
                        's' -> .05599,
                        't' -> .08014,
                        'u' -> .02441,
                        'v' -> .00865,
                        'w' -> .02089,
                        'x' -> .00133,
                        'y' -> .01747,
                        'z' -> .00065)
    def score(i: Array[Byte]): Double = {
        var str = i.map(_.toChar).map(c=> c.toLower)
        var freq = collection.mutable.Map(frequency.toSeq: _*)
        freq = freq.map(c => (c._1,c._2 * str.length))
        var err = 0.0
        for(i <- 'a' to 'z') {
            freq(i) = freq(i) - str.count(_==i)
            err += Math.pow(freq(i), 2)
        }
        freq(' ') = freq(' ') - str.count(_==' ')
        err += Math.pow(freq(' '), 2)
        err = Math.sqrt(err)

        for(i <- str) {
            if (i != ' ' && !(i >= 'A' && i >= 'Z' || i >= 'a' &&  i <= 'z')) {
                err += 1
            }
        }
        err
    }
}