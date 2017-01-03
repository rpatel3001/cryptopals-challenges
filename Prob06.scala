import Prob02._
import Prob03._
import java.util.Base64

object Prob06 {
	def main(args: Array[String]): Unit = {
		val str1 = "this is a test".getBytes
		val str2 = "wokka wokka!!!".getBytes
		val out = hamming(str1, str2)
		if (out != 37) {
			println("Prob 06: Fail")
			return
		}

		val base64 = io.Source.fromFile("data6.txt").mkString
		val bytes = Base64.getMimeDecoder().decode(base64)

		var editdist = collection.mutable.ListBuffer[(Int, Double)]()
		for (keysize ← 1 to 40) {
			val blocks = collection.mutable.ListBuffer[Array[Byte]]()
			for (i ← 0 until bytes.length / keysize) {
				blocks += bytes.slice(keysize * i, keysize * i + keysize)
			}
			var num = 0
			var sum = 0.0
			for (i ← 0 until blocks.length / 2) {
				num += 1
				sum += hamming(blocks(i * 2), blocks(i * 2 + 1))
			}
			editdist += keysize -> sum / num / keysize
		}
		editdist = editdist.sortBy(_._2).slice(0, 5)

		var keytext = Array[Byte]()
		var text = ""
		var minscore = 1000000.0
		for (key ← editdist) {
			val keysize = key._1
			val blocks = collection.mutable.ArrayBuffer[Array[Byte]]()
			for (i ← 0 until bytes.length / keysize) {
				blocks += bytes.slice(keysize * i, keysize * i + keysize)
			}

			val trans = collection.mutable.ArrayBuffer[Array[Byte]]()
			for (i ← 0 until keysize) {
				val s = collection.mutable.ArrayBuffer[Byte]()
				for (b ← blocks) {
					if (i < b.length) {
						s.append(b(i))
					}
				}
				trans.append(s.toArray)
			}

			val k = (for (b ← trans) yield {
				decrypt(b)._1
			}).slice(0, keysize).toArray
			//println(toASCII(keytext))
			val plain = keyXOR(bytes, k)
			val s = score(plain)
			if (s < minscore) {
				keytext = k
				minscore = s
				text = toASCII(plain)
			}
		}
		//println("Prob 6: " + minscore + " " + toASCII(keytext))

		if (toASCII(keytext) == "Terminator X: Bring the noise") {
			println("Prob 06: Success")
		} else {
			println("Prob 06: Fail")
		}
	}

	def hamming(str1: Array[Byte], str2: Array[Byte]): Int = {
		BigInt(xor(str1, str2)).toString(2).count(_ == '1')
	}
}