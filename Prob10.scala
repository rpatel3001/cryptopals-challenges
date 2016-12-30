import java.util.Base64
import Prob07.decodeAESECB
import Prob09.padPKCS7
import Prob02.xor
import Prob03.toASCII
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec;
import javax.crypto.spec.IvParameterSpec;

object Prob10 {
	def main(args: Array[String]) = {
		var base64 = io.Source.fromFile("data10.txt").mkString
		var bytes = Base64.getMimeDecoder().decode(base64)
		var iv = Array.fill[Byte](16)(0)
		var key = "YELLOW SUBMARINE".getBytes
		var decoded = decodeAESCBC(bytes, key, iv)
		var encoded = encodeAESCBC(decoded, key, iv)
		if(toASCII(encoded) == toASCII(bytes) && toASCII(decoded).slice(0,33) == "I'm back and I'm ringin' the bell") {
			println("Prob 10: Success")
		} else {
			println("Prob 10: Fail")
		}
	}

	def unpadPKCS7(str: Array[Byte]): Array[Byte] = {
		str.dropRight(str.last.toInt)
	}

	def decodeAESCBC(str: Array[Byte], key: Array[Byte], iv: Array[Byte]): Array[Byte] = {
		var blocks = str.grouped(16).toArray
		var out = collection.mutable.ArrayBuffer[Byte]()
		out ++= xor(decodeAESECB(blocks(0), key), iv)
		for(i <- 1 until blocks.size) {
			out ++= xor(decodeAESECB(blocks(i), key), blocks(i-1))
		}
		unpadPKCS7(out.toArray)
	}

	def encodeAESCBC(str: Array[Byte], key: Array[Byte], iv: Array[Byte]): Array[Byte] = {
		var blocks = padPKCS7(str, 16).grouped(16)
		var out = collection.mutable.ArrayBuffer[Byte]()
		var prev = iv
		for(block <- blocks) {
			var b = xor(block, prev)
			var o = encodeAESECB(b, key)
			out ++= o
			prev = o
		}
		out.toArray
	}

	def encodeAESECB(str: Array[Byte], key: Array[Byte]): Array[Byte] = {
		var cipher = Cipher.getInstance("AES/ECB/NoPadding")
		cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "AES"))
		cipher.doFinal(str)
	}
}