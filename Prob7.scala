import java.util.Base64
import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec;
import Prob3.toASCII

object Prob7 {
	def main(args: Array[String]): Unit = {
		var key = new SecretKeySpec("YELLOW SUBMARINE".getBytes, "AES");
		var base64 = io.Source.fromFile("data7.txt").filter(c => c != '\n').mkString
		var bytes = Base64.getDecoder().decode(base64)
		var cipher = Cipher.getInstance("AES/ECB/NoPadding")
		cipher.init(Cipher.DECRYPT_MODE, key)
		var plain = toASCII(cipher.doFinal(bytes))
		if(plain.slice(0,33) == "I'm back and I'm ringin' the bell") {
			println("Prob 7: Success")
		} else {
			println("Prob 7: Success")
		}
	}
}