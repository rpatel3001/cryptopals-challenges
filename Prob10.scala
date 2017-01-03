import java.util.Base64
import Prob07.decodeAESECB
import Prob09.padPKCS7
import Prob02.xor
import Prob03.toASCII
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec;
import Prob15.unpadPKCS7

object Prob10 {
	def main(args: Array[String]) = {
		val base64 = io.Source.fromFile("data10.txt").mkString
		val bytes = Base64.getMimeDecoder().decode(base64)
		val iv = Array.fill[Byte](16)(0)
		val key = "YELLOW SUBMARINE".getBytes
		val decoded = decodeAESCBC(bytes, key, iv)
		val encoded = encodeAESCBC(decoded, key, iv)
		if (toASCII(encoded) == toASCII(bytes) && toASCII(decoded).slice(0, 33) == "I'm back and I'm ringin' the bell") {
			println("Prob 10: Success")
		} else {
			println("Prob 10: Fail")
		}
	}

	def decodeAESCBC(str: Array[Byte], key: Array[Byte], iv: Array[Byte]): Array[Byte] = {
		val blocks = str.grouped(16).toArray
		val out = collection.mutable.ArrayBuffer[Byte]()
		out ++= xor(decodeAESECB(blocks(0), key), iv)
		for (i ← 1 until blocks.size) {
			out ++= xor(decodeAESECB(blocks(i), key), blocks(i - 1))
		}
		unpadPKCS7(out.toArray)
	}

	def encodeAESCBC(str: Array[Byte], key: Array[Byte], iv: Array[Byte]): Array[Byte] = {
		val blocks = padPKCS7(str, 16).grouped(16)
		val out = collection.mutable.ArrayBuffer[Byte]()
		var prev = iv
		for (block ← blocks) {
			val o = encodeAESECB(xor(block, prev), key)
			out ++= o
			prev = o
		}
		out.toArray
	}

	def encodeAESECB(str: Array[Byte], key: Array[Byte]): Array[Byte] = {
		val cipher = Cipher.getInstance("AES/ECB/NoPadding")
		cipher.init(Cipher.ENCRYPT_MODE, new SecretKeySpec(key, "AES"))
		cipher.doFinal(str)
	}
}