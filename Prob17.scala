import Prob11.keygen
import Prob10.encodeAESCBC
import Prob10.decodeAESCBC
import Prob15.unpadPKCS7
import java.util.Base64

object Prob17 {
  def main(args: Array[String]): Unit = {
    if (strings.count(_ == Base64.getEncoder.encodeToString(breakCBC)) == 1) {
      println("Prob 17: Success")
    } else {
      println("Prob 17: Fail")
    }
  }

  def breakCBC(): Array[Byte] = {
    val blocksize = 16
    val (cipher, iv) = encrypt
    val ciphblocks = cipher.grouped(16).toArray
    val plainblocks = Array.fill(ciphblocks.size)(Array.fill[Byte](16)(0))
    for (iblock ← 0 until ciphblocks.size) {
      val cn = ciphblocks(iblock)
      val prevblock = {
        if (iblock == 0) iv
        else ciphblocks(iblock - 1)
      }
      for (ibyte ← 15 to 0 by -1) {
        val cprime = Array.fill[Byte](blocksize)(0)
        for (i ← ibyte + 1 until blocksize) {
          cprime.update(i, ((16 - ibyte) ^ plainblocks(iblock)(i) ^ prevblock(i)).toByte)
        }
        var x = 0
        while (!validPadding(cn, cprime)) {
          x += 1
          cprime.update(ibyte, x.toByte)
        }
        val plain = (16 - ibyte) ^ x ^ prevblock(ibyte)
        plainblocks(iblock).update(ibyte, plain.toByte)
      }
    }
    unpadPKCS7(plainblocks.flatten)
  }

  val strings = List(
    "MDAwMDAwTm93IHRoYXQgdGhlIHBhcnR5IGlzIGp1bXBpbmc=",
    "MDAwMDAxV2l0aCB0aGUgYmFzcyBraWNrZWQgaW4gYW5kIHRoZSBWZWdhJ3MgYXJlIHB1bXBpbic=",
    "MDAwMDAyUXVpY2sgdG8gdGhlIHBvaW50LCB0byB0aGUgcG9pbnQsIG5vIGZha2luZw==",
    "MDAwMDAzQ29va2luZyBNQydzIGxpa2UgYSBwb3VuZCBvZiBiYWNvbg==",
    "MDAwMDA0QnVybmluZyAnZW0sIGlmIHlvdSBhaW4ndCBxdWljayBhbmQgbmltYmxl",
    "MDAwMDA1SSBnbyBjcmF6eSB3aGVuIEkgaGVhciBhIGN5bWJhbA==",
    "MDAwMDA2QW5kIGEgaGlnaCBoYXQgd2l0aCBhIHNvdXBlZCB1cCB0ZW1wbw==",
    "MDAwMDA3SSdtIG9uIGEgcm9sbCwgaXQncyB0aW1lIHRvIGdvIHNvbG8=",
    "MDAwMDA4b2xsaW4nIGluIG15IGZpdmUgcG9pbnQgb2g=",
    "MDAwMDA5aXRoIG15IHJhZy10b3AgZG93biBzbyBteSBoYWlyIGNhbiBibG93")

  val globalkey = keygen
  def encrypt(): (Array[Byte], Array[Byte]) = {
    val iv = keygen
    val text = Base64.getDecoder.decode(scala.util.Random.shuffle(strings).head)
    (encodeAESCBC(text, globalkey, iv), iv)
  }

  def printByteArray(data: Array[Byte]): Unit = {
    println(data.grouped(16).toArray.map(_.mkString).mkString(" "))
  }

  def validPadding(data: Array[Byte], iv: Array[Byte]): Boolean = {
    try {
      decodeAESCBC(data, globalkey, iv)
      return true
    } catch {
      case e: Exception ⇒ false
    }
  }
}