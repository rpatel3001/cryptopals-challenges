import Prob11.keygen
import Prob10.encodeAESCBC
import Prob10.decodeAESCBC
import Prob03.toASCII
import Prob03.toASCII

object Prob16 {
	def main(args: Array[String]): Unit = {
		if (isAdmin(makeAdminCipher) && !isAdmin(encrypt(";admin=true".getBytes))) {
			println("Prob 16: Success")
		} else {
			println("Prob 16: Fail")
		}
	}

	def makeAdminCipher(): Array[Byte] = {
		val blocksize = 16

		var presize = 16
		var prev = encrypt(Array()).mkString
		for (i ← 0 to blocksize) {
			val encr = encrypt(Array.fill[Byte](i)(0)).slice(0, blocksize).mkString
			if (encr == prev) {
				presize -= 1
			}
			prev = encr
		}
		val prepad = Array.fill[Byte](presize)('X'.toByte)

		val blocks1 = encrypt(prepad ++ ("Y" * blocksize).getBytes).grouped(blocksize).map(_.mkString).toArray
		val blocks2 = encrypt(prepad ++ ("Z" * blocksize).getBytes).grouped(blocksize).map(_.mkString).toArray
		val blocknum = blocks1.zip(blocks2).zipWithIndex.filter(c ⇒ c._1._1 != c._1._2).head._2 + 1

		var cipher = encrypt(prepad ++ Array.fill[Byte](blocksize)('Y'.toByte) ++ "9admin9true".getBytes)
		val len = cipher.size * 8
		cipher = bitflip(cipher, len - 8 * blocksize * blocknum + 8 * 9 + 2)
		cipher = bitflip(cipher, len - 8 * blocksize * blocknum + 8 * 15 + 1)
		cipher
	}

	def bitflip(data: Array[Byte], i: Int): Array[Byte] = {
		val b = data.reverse.apply(i / 8) ^ (1 << (i % 8))
		data.reverse.updated(i / 8, b.toByte).reverse
	}

	def byteToBinary(b: Byte): String = {
		val str = b.toBinaryString
		if (str.size == 32) {
			str.slice(24, 32)
		} else {
			"0" * (8 - str.size) + str
		}
	}

	var globalkey = keygen
	var iv = keygen
	def encrypt(data: Array[Byte]): Array[Byte] = {
		val pre = "comment1=cooking%20MCs;userdata=".getBytes
		val post = ";comment2=%20like%20a%20pound%20of%20bacon".getBytes
		encodeAESCBC(pre ++ data.filter(c ⇒ c != ';' && c != '=') ++ post, globalkey, iv)
	}

	def isAdmin(str: Array[Byte]): Boolean = {
		val decr = toASCII((decodeAESCBC(str, globalkey, iv))).split(";").map(_.split("=")).collect({ case Array(a, b) ⇒ (a, b) }).toMap
		decr.contains("admin") && decr("admin") == "true"
	}
}