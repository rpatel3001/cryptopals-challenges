import scala.util.Random
import Prob03.toASCII
import Prob08.detectECBRepeats
import Prob07.decodeAESECB
import Prob10._

object Prob11 {
	def main(args: Array[String]): Unit = {
		var data = ("A" * 128).getBytes
		for(i <- 0 to 100) {
			var encr = randomEncrypt(data)
			if(getAESMode(encr._1) != encr._2) {
				println("Prob 11: Fail")
				return
			}
		}
		println("Prob 11: Success")
	}

	def getAESMode(data: Array[Byte]): String = {
		if(detectECBRepeats(data) > 0) {
			"ECB"
		} else {
			"CBC"
		}
	}

	def randomEncrypt(data: Array[Byte]): (Array[Byte], String) = {
		var c1 = Random.nextInt(5) + 5
		var c2 = Random.nextInt(5) + 5
		var d = Random.nextString(c1).getBytes ++ data ++ Random.nextString(c2).getBytes
		if(Random.nextInt(2) == 0) {
			(encodeAESECB(data, keygen), "ECB")
		} else {
			(encodeAESCBC(data, keygen, keygen), "CBC")
		}
	}

	def keygen(): Array[Byte] = {
		var bytes = new Array[Byte](16)
		Random.nextBytes(bytes)
		bytes
	}
}