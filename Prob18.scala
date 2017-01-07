import java.util.Base64
import Prob10.encodeAESECB
import Prob02.xor

object Prob18 {
  def main(args: Array[String]): Unit = {
    val ciph = Base64.getDecoder.decode("L77na/nrFsKvynd6HzOoG7GHTLXsTVu9qvY/2syLXzhPweyyMTJULu/6/kXX0KSvoOLSFQ==")
    val ans = "Yo, VIP Let's kick it Ice, Ice, baby Ice, Ice, baby ".getBytes
    val out = AESCTRTransform(ciph, "YELLOW SUBMARINE".getBytes, Array.fill[Byte](8)(0))
    if (out.mkString == ans.mkString) {
      println("Prob 18: Success")
    } else {
      println("Prob 18: Fail")
    }
  }

  def AESCTRTransform(data: Array[Byte], key: Array[Byte], nonce: Array[Byte]): Array[Byte] = {
    data.grouped(16).zipWithIndex.map(c => xor(c._1, encodeAESECB(nonce ++ padCounter(BigInt(c._2).toByteArray).reverse, key))).flatten.toArray
  }

  def padCounter(ctr: Array[Byte]): Array[Byte] = {
    Array.fill[Byte](8-ctr.size)(0) ++ ctr
  }
}