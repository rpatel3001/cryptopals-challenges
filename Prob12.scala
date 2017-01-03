import Prob11._
import Prob03.toASCII
import Prob10.encodeAESECB
import Prob09.padPKCS7
import java.util.Base64

object Prob12 {
	def main(args: Array[String]): Unit = {
		val blocksize = getBlockSize
		val mode = getAESMode(unknownKeyECB(("A" * blocksize * 2).getBytes))
		//println(blocksize + " " + mode)
		if (blocksize != 16 || mode != "ECB") {
			println("Prob 12: Fail")
			return
		}
		val hidden = breakAESECB(blocksize)
		if (toASCII(hidden).slice(0, 17) == "Rollin' in my 5.0") {
			println("Prob 12: Success")
		} else {
			println("Prob 12: Fail")
		}
	}

	def breakAESECB(blocksize: Int): Array[Byte] = {
		val known = collection.mutable.ArrayBuffer[Byte]()
		var currentblock = collection.mutable.ArrayBuffer[Byte]()
		var datasize = unknownKeyECB(Array()).length
		var pad = 0
		for (i ← 1 to blocksize) {
			val arr = Array.fill[Byte](i)(1)
			val len = unknownKeyECB(arr).length
			if (len == datasize) {
				pad += 1
			}
		}
		pad += 1
		datasize -= pad
		while (known.size + currentblock.size < datasize) {
			val testbytes = (("A" * (blocksize - 1 - currentblock.size)).getBytes).toArray
			val predict = (0 to 256).map(c ⇒ (c.toByte, (testbytes ++ known ++ currentblock :+ c.toByte)))
			val dict = predict.map(c ⇒ (c._1, unknownKeyECB(c._2).slice(0, known.size + blocksize)))
			val encr = unknownKeyECB(testbytes).slice(0, known.size + blocksize)
			val char = dict.filter(_._2.mkString == encr.mkString).apply(0)
			currentblock += char._1
			if (currentblock.size == blocksize) {
				known ++= currentblock
				currentblock = collection.mutable.ArrayBuffer[Byte]()
			}
		}
		(known ++ currentblock).toArray
	}

	var globalkey = keygen
	def unknownKeyECB(data: Array[Byte]): Array[Byte] = {
		val base64 = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
		val bytes = Base64.getMimeDecoder().decode(base64)
		encodeAESECB(padPKCS7(data ++ bytes, 16), globalkey)
	}

	def getBlockSize(): Int = {
		for (blocksize ← 2 to 128) {
			val encr = unknownKeyECB(("A" * blocksize * 2).getBytes)
			if (encr.slice(0, blocksize).mkString == encr.slice(blocksize, blocksize * 2).mkString) {
				return blocksize
			}
		}
		-1
	}
}