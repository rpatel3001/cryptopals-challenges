import java.util.Base64
import Util._
import util.Random

object Prob14 {
  def main(args: Array[String]): Unit = {
    val blocksize = getBlockSize
    if (blocksize != 16) {
      println("Prob 14: Fail")
      return
    }
    val hidden = breakPrefixAESECB(blocksize)
    if (toASCII(hidden).slice(0, 17) == "Rollin' in my 5.0") {
      println("Prob 14: Success")
    } else {
      println("Prob 14: Fail")
    }
  }

  def breakPrefixAESECB(blocksize: Int): Array[Byte] = {
    val known = collection.mutable.ArrayBuffer[Byte]()
    var currentblock = collection.mutable.ArrayBuffer[Byte]()

    var presize = 16
    var prev = unknownKeyPrefixECB(Array()).mkString
    for (i ← 0 to blocksize) {
      val encr = unknownKeyPrefixECB(Array.fill[Byte](i)(0)).slice(0, blocksize).mkString
      if (encr == prev) {
        presize -= 1
      }
      prev = encr
    }

    val datasize = unknownKeyPrefixECB(Array.fill[Byte](presize)(0)).size
    var pad = 0
    for (i ← 1 to blocksize) {
      val arr = Array.fill[Byte](i + presize)(0)
      val len = unknownKeyPrefixECB(arr).size
      if (len == datasize) {
        pad += 1
      }
    }
    pad += 1
    val prepad = Array.fill[Byte](presize)('X'.toByte)
    val blocks = unknownKeyPrefixECB(prepad ++ ("Z" * blocksize * 2).getBytes).grouped(blocksize).zipWithIndex.map({ case (v, i) ⇒ (i, v) }).toArray
    val blocknum = blocks.dropRight(1).filter({ case (i, v) ⇒ v.mkString == blocks(i + 1)._2.mkString })(0)._1
    while (pad + blocksize * blocknum + known.size + currentblock.size < datasize) {
      val testbytes = ("A" * (blocksize - 1 - currentblock.size)).getBytes
      val predict = (0 to 256).map(c ⇒ (c.toByte, (testbytes ++ known ++ currentblock :+ c.toByte)))
      val dict = predict.map(c ⇒ (c._1, unknownKeyPrefixECB(prepad ++ c._2).slice(blocksize * blocknum, blocksize * blocknum + known.size + blocksize)))
      val encr = unknownKeyPrefixECB(prepad ++ testbytes).slice(blocksize * blocknum, blocksize * blocknum + known.size + blocksize)
      val chardict = dict.filter(_._2.mkString == encr.mkString)
      val char = chardict.apply(0)._1
      currentblock += char
      if (currentblock.size == blocksize) {
        known ++= currentblock
        currentblock = collection.mutable.ArrayBuffer[Byte]()
      }
    }
    (known ++ currentblock).toArray
  }

  def getBlockSize(): Int = {
    val base = unknownKeyPrefixECB(Array()).size
    for (blocksize ← 1 to 128) {
      val len = unknownKeyPrefixECB(("A" * blocksize).getBytes).size
      if (len != base) {
        return len - base
      }
    }
    -1
  }

  var globalkey = keygen
  var prefix = (0 to Random.nextInt(16) map { c ⇒ Random.nextInt(256).toByte }).toArray
  def unknownKeyPrefixECB(data: Array[Byte]): Array[Byte] = {
    val base64 = "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkgaGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBqdXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUgYnkK"
    val bytes = Base64.getMimeDecoder().decode(base64)
    encodeAESECB(padPKCS7(prefix ++ data ++ bytes, 16), globalkey)
  }
}