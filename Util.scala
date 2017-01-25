import javax.crypto.Cipher
import javax.crypto.spec.SecretKeySpec;
import util.Random

object Util {
  def toHex(str: Array[Byte]): String = {
    str.map(byteToHex).mkString
  }

  def byteToHex(b: Byte): String = {
    if (b < 0x10) {
      "0" + b.toHexString
    } else {
      b.toHexString
    }
  }

  def xor(val1: Array[Byte], val2: Array[Byte]): Array[Byte] = {
    val ret = collection.mutable.ArrayBuffer[Byte]()
    for (i ← 0 until Math.min(val1.size, val2.size)) {
      ret.append((val1(i) ^ val2(i)).toByte)
    }
    ret.toArray
  }

  def toASCII(raw: Array[Byte]): String = {
    raw.map(_.toChar).mkString
  }

  def decryptVignere(str: Array[Byte]): (Byte, Double) = {
    val results: collection.mutable.Map[Byte, Double] = collection.mutable.Map()
    for (char ← 0 to 255) {
      var tmp = str
      tmp = keyXOR(tmp, Array(char.toByte))
      results += char.toByte -> score(tmp)
    }
    results.toSeq.sortBy(_._2).head
  }

  val frequency = Array(
    ' ' -> .11504,
    'a' -> .07227,
    'b' -> .01320,
    'c' -> .02462,
    'd' -> .03764,
    'e' -> .11241,
    'f' -> .01972,
    'g' -> .01783,
    'h' -> .05393,
    'i' -> .06165,
    'j' -> .00135,
    'k' -> .00683,
    'l' -> .03562,
    'm' -> .02129,
    'n' -> .05973,
    'o' -> .06643,
    'p' -> .01707,
    'q' -> .00084,
    'r' -> .05298,
    's' -> .05599,
    't' -> .08014,
    'u' -> .02441,
    'v' -> .00865,
    'w' -> .02089,
    'x' -> .00133,
    'y' -> .01747,
    'z' -> .00065).sortBy(_._2).reverse
  def score(i: Array[Byte]): Double = {
    val str = i.map(_.toChar).map(c ⇒ c.toLower)
    var freq = collection.mutable.Map(frequency.toSeq: _*)
    freq = freq.map(c ⇒ (c._1, c._2 * str.length))
    var err = 0.0
    for (i ← 'a' to 'z') {
      freq(i) = freq(i) - str.count(_ == i)
      err += Math.pow(freq(i), 2)
    }
    freq(' ') = freq(' ') - str.count(_ == ' ')
    err += Math.pow(freq(' '), 2)
    err = Math.sqrt(err)

    for (i ← str) {
      if (i != ' ' && !(i >= 'A' && i <= 'Z' || i >= 'a' && i <= 'z')) {
        err += 20
      }
      if (i > 126 || i < 32) {
        err += 100
      }
    }
    err
  }

  def keyXOR(str: Array[Byte], key: Array[Byte]): Array[Byte] = {
    val len = str.length
    val k = Array.fill(len / key.length + 1)(key).flatten.slice(0, len)
    xor(str, k)
  }

  def breakXOR(bytes: Array[Byte], keysize: Int): (Int, Array[Byte], Double, String) = {
    val blocks = bytes.grouped(keysize).toArray
    val trans = collection.mutable.ArrayBuffer[Array[Byte]]()
    for (i ← 0 until keysize) {
      val s = collection.mutable.ArrayBuffer[Byte]()
      for (b ← blocks) {
        if (i < b.length) {
          s.append(b(i))
        }
      }
      trans.append(s.toArray)
    }
    val k = (for (b ← trans) yield {
      decryptVignere(b)._1
    }).slice(0, keysize).toArray
    val plain = keyXOR(bytes, k)
    (keysize, k, score(plain), toASCII(plain))
  }

  def hamming(str1: Array[Byte], str2: Array[Byte]): Int = {
    BigInt(xor(str1, str2)).toString(2).count(_ == '1')
  }

  def decodeAESECB(str: Array[Byte], key: Array[Byte]): Array[Byte] = {
    val cipher = Cipher.getInstance("AES/ECB/NoPadding")
    cipher.init(Cipher.DECRYPT_MODE, new SecretKeySpec(key, "AES"))
    cipher.doFinal(str)
  }

  def padPKCS7(str: Array[Byte], size: Int): Array[Byte] = {
    val pad = size - str.size % size
    str ++ (pad.toChar.toString * pad).getBytes
  }

  def unpadPKCS7(str: Array[Byte]): Array[Byte] = {
    val pad = str.last
    val padding = str.reverse.slice(0, pad)
    if (pad <= 0 || pad > 16 && padding.size != pad || padding.count(_ != pad) > 0) {
      throw new RuntimeException("bad padding")
    }
    str.dropRight(pad)
  }

  def detectECBRepeats(str: Array[Byte]): Int = {
    val blocks = str.grouped(16).toList.map(_.mkString)
    blocks.size - blocks.toSet.size
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

  def keygen(): Array[Byte] = {
    val bytes = new Array[Byte](16)
    Random.nextBytes(bytes)
    bytes
  }

  def getAESMode(data: Array[Byte]): String = {
    if (detectECBRepeats(data) > 0) {
      "ECB"
    } else {
      "CBC"
    }
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

  def printByteArray(data: Array[Byte]): Unit = {
    println(data.grouped(16).toArray.map(_.mkString).mkString(" "))
  }

  def AESCTRTransform(data: Array[Byte], key: Array[Byte], nonce: Array[Byte]): Array[Byte] = {
    data.grouped(16).zipWithIndex.map(c => xor(c._1, encodeAESECB(nonce ++ padCounter(BigInt(c._2).toByteArray).reverse, key))).flatten.toArray
  }

  def padCounter(ctr: Array[Byte]): Array[Byte] = {
    Array.fill[Byte](8-ctr.size)(0) ++ ctr
  }
}