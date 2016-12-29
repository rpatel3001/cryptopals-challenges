import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec;
import Prob3.toASCII

object Prob7 {
	def main(args: Array[String]): Unit = {
		var base64 = io.Source.fromFile("data7.txt").filter(c => c != '\n').mkString
		var bytes = Base64.getDecoder().decode(base64)
		if(toASCII(AESdecode(bytes, "YELLOW SUBMARINE".getBytes)).slice(0,33) == "I'm back and I'm ringin' the bell") {
			println("Prob 7: Success")
		} else {
			println("Prob 7: Success")
		}
	}

	def AESdecode(str: Array[Byte], key: Array[Byte]): Array[Byte] = {
		var seckey = new SecretKeySpec(key, "AES");
		var cipher = Cipher.getInstance("AES/ECB/NoPadding")
		cipher.init(Cipher.DECRYPT_MODE, seckey)
		cipher.doFinal(str)
	}
}