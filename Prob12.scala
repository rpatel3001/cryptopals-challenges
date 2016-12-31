import Prob11._
import Prob02.byteToHex
import Prob03.toASCII
import Prob10.encodeAESECB
import Prob09.padPKCS7
import java.util.Base64

object Prob12 {
	def main(args: Array[String]): Unit = {
		var blocksize = getBlockSize
		var mode = getAESMode(unknownKeyECB(("A" * blocksize * 2).getBytes))
		//println(blocksize + " " + mode)
		if(blocksize != 16 || mode != "ECB") {
			println("Prob 12: Fail")
			return
		}
		var hidden = breakAESECB(blocksize)
		if(toASCII(hidden).slice(0,17) == "Rollin' in my 5.0") {
			println("Prob 12: Success")
		} else {
			println("Prob 12: Fail")
		}
	}

	def breakAESECB(blocksize: Int): Array[Byte] = {
		var known = collection.mutable.ArrayBuffer[Byte]()
		var currentblock = collection.mutable.ArrayBuffer[Byte]()
		var datasize = unknownKeyECB(Array()).length
		var pad = 0
		for(i <- 1 to blocksize) {
			var arr = Array.fill[Byte](i)(1)
			var len = unknownKeyECB(arr).length
			if(len == datasize) {
				pad += 1
			}
		}
		datasize -= pad + 1
		while(known.size + currentblock.size < datasize) {
			var testbytes = (("A" * (blocksize - 1 - currentblock.size)).getBytes).toArray
			var predict = (0 to 256).map(c => (c.toByte, (testbytes ++ known ++currentblock :+ c.toByte)))
			var dict = predict.map(c => (c._1, unknownKeyECB(c._2).slice(0, known.size + blocksize)))
			var encr = unknownKeyECB(testbytes).slice(0, known.size + blocksize)
			var char = dict.filter(_._2.mkString == encr.mkString).apply(0)
			var clear = toASCII(predict.filter(char._1 == _._1).apply(0)._2)
			currentblock += char._1
			if(currentblock.size == blocksize) {
				known ++= currentblock
				currentblock = collection.mutable.ArrayBuffer[Byte]()
			}
		}
		(known ++ currentblock).toArray
	}
	
	var globalkey = keygen
	def unknownKeyECB(data: Array[Byte]): Array[Byte] = {
		var base64 = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
		var bytes = Base64.getMimeDecoder().decode(base64)
		encodeAESECB(padPKCS7(data ++ bytes, 16), globalkey)
	}

	def getBlockSize(): Int = {
		for(blocksize <- 2 to 128) {
			var encr = unknownKeyECB(("A" * blocksize * 2).getBytes)
			if(encr.slice(0,blocksize).mkString == encr.slice(blocksize, blocksize*2).mkString) {
				return blocksize
			}
		}
		-1
	}
}