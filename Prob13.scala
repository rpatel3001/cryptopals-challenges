import Prob11.keygen
import Prob10.encodeAESECB
import Prob07.decodeAESECB
import Prob03.toASCII
import Prob09.padPKCS7
import Prob15.unpadPKCS7

object Prob13 {
  def main(args: Array[String]) {
    val test1 = "foo=bar&baz=qux&zap=zazzle"
    val test1ans = Map("foo" -> "bar", "baz" -> "qux", "zap" -> "zazzle")
    val test2 = "foo@bar.com"
    val test2ans = "email=foo@bar.com&uid=10&role=user"
    if (keyval(test1) != test1ans || profile_for(test2) != test2ans) {
      println("Prob 13: Fail")
      return
    }
    val encradmin = makeAdminProfile
    val adminprofile = decryptUser(encradmin)
    if (adminprofile("role") == "admin") {
      println("Prob 13: Success")
    } else {
      println("Prob 13: Fail")
    }
  }

  def makeAdminProfile(): Array[Byte] = {
    var padding = ""
    val datasize = encryptedUser("").length
    val blocksize = 16
    var pad = 0
    for (i ← 0 to 16) {
      val len = encryptedUser("A" * i).length
      if (len == datasize) {
        pad += 1
      }
    }
    pad += 1
    padding += "A" * pad

    val blocks = encryptedUser(padding + "A" * blocksize * 2).grouped(blocksize).zipWithIndex.map({ case (v, i) ⇒ (i, v.mkString) }).toArray
    val blocknum = blocks.dropRight(1).filter({ case (i, v) ⇒ v == blocks(i + 1)._2 })(0)._1
    val inject = toASCII(padPKCS7("admin".getBytes, blocksize))
    val roleblock = encryptedUser(padding + inject).grouped(blocksize).toArray.apply(blocknum)
    val profile = encryptedUser(padding + "B" * ("user".size - 1))
    profile.dropRight(blocksize) ++ roleblock
  }

  var globalkey = keygen
  def encryptedUser(email: String): Array[Byte] = {
    val prof = profile_for(email)
    encodeAESECB(padPKCS7(prof.getBytes, 16), globalkey)
  }

  def decryptUser(encr: Array[Byte]): Map[String, String] = {
    keyval(toASCII(unpadPKCS7(decodeAESECB(encr, globalkey))))
  }

  def keyval(str: String): Map[String, String] = {
    str.split("&").map(_.split("=")).collect({ case Array(a, b) ⇒ (a, b) }).toMap
  }

  def profile_for(email: String): String = {
    val sanitized = email.filter(_ != '&').filter(_ != '=').mkString
    Map("email" -> sanitized, "uid" -> "10", "role" -> "user").map(_.productIterator.mkString("=")).mkString("&")
  }
}