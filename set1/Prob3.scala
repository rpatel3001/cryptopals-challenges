object SingleByteXOR {
    def main(args: Array[String]): Unit = {
        var str = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"
        str = toASCII(str)
        var results: collection.mutable.Map[Char, Double] = collection.mutable.Map()
        for (char <- 'A' to 'z') {
            var tmp = str
            tmp = tmp.map(n => (n ^ char).toChar)
            results += char -> score(tmp)
        }
        var sorted = results.toSeq.sortBy(_._2)
        for(i <- 0 to 0) {
            println(sorted(i) + " " + str.map(c => (c ^ sorted(i)._1).toChar))
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
            //println(str(i*2) + " " + str(i*2+1))
            //println((hexval(str(i*2)) << 4) + " " + hexval(str(i*2+1)))
            ascii.append(((hexval(str(i*2)) << 4) + hexval(str(i*2+1))).toChar)
        }
        ascii.toString
    }

    var frequency = Map(' ' -> .13000,
                        'a' -> .08167,
                        'b' -> .01492,
                        'c' -> .02782,
                        'd' -> .04253,
                        'e' -> .12702,
                        'f' -> .02228,
                        'g' -> .02015,
                        'h' -> .06094,
                        'i' -> .06966,
                        'j' -> .00153,
                        'k' -> .00772,
                        'l' -> .04025,
                        'm' -> .02406,
                        'n' -> .06749,
                        'o' -> .07507,
                        'p' -> .01929,
                        'q' -> .00095,
                        'r' -> .05987,
                        's' -> .06327,
                        't' -> .09056,
                        'u' -> .02758,
                        'v' -> .00978,
                        'w' -> .02360,
                        'x' -> .00150,
                        'y' -> .01975,
                        'z' -> .00074)
    def score(s: String): Double = {
        var str = s.map(c=> c.toLower)
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
            if (i != ' ' && (i < 'A' || i > 'z')) {
                err += 1
            }
        }
        err
    }
}