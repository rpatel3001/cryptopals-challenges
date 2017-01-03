import Prob02._

object Prob03 {
	def main(args: Array[String]): Unit = {
		val str = BigInt("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736", 16).toByteArray
		val res = decrypt(str)
		//println("Prob 3: " + res + " " + toASCII(str).map(c => (c ^ res._1).toChar))
		if (toASCII(str).map(c ⇒ (c ^ res._1).toChar) == "Cooking MC's like a pound of bacon") {
			println("Prob 03: Success")
		} else {
			println("Prob 03: Fail")
		}
	}

	def decrypt(str: Array[Byte]): (Byte, Double) = {
		val results: collection.mutable.Map[Byte, Double] = collection.mutable.Map()
		for (char ← 32 to 126) {
			var tmp = str
			tmp = keyXOR(tmp, Array(char.toByte))
			results += char.toByte -> score(tmp)
		}
		results.toSeq.sortBy(_._2) apply 0
	}

	def toASCII(raw: Array[Byte]): String = {
		raw.map(_.toChar).mkString
	}

	def keyXOR(str: Array[Byte], key: Array[Byte]): Array[Byte] = {
		val len = str.length
		val k = Array.fill(len / key.length + 1)(key).flatten.slice(0, len)
		xor(str, k)
	}

	val frequency = Map(
		' ' -> .11504,
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
		val str = i.map(_.toChar).map(c ⇒ c.toLower)
		var freq = collection.mutable.Map(frequency.toSeq: _*)
		freq = freq.map(c ⇒ (c._1, c._2 * str.length))
		var err = 0.0
		for (i ← 'a' to 'z') {
			freq(i) = freq(i) - str.count(_ == i)
			err += Math.pow(freq(i), 2)
		}
		freq(' ') = freq(' ') - str.count(_ == ' ')
		err += Math.pow(freq(' '), 2)
		err = Math.sqrt(err)

		for (i ← str) {
			if (i != ' ' && !(i >= 'A' && i >= 'Z' || i >= 'a' && i <= 'z')) {
				err += 1
			}
		}
		err
	}
}