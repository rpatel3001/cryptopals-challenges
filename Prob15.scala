object Prob15 {
  def main(args: Array[String]): Unit = {
    val test1 = "ICE ICE BABY".getBytes ++ Array.fill[Byte](4)(4)
    val test2 = "ICE ICE BABY".getBytes ++ Array.fill[Byte](4)(5)
    val test3 = "ICE ICE BABY".getBytes ++ Array[Byte](1, 2, 3, 4)
    val ans1 = "ICE ICE BABY".getBytes.mkString == unpadPKCS7(test1).mkString
    var ans2 = false
    var ans3 = false
    try {
      unpadPKCS7(test2)
    } catch {
      case e: Exception ⇒ {
        ans2 = true
      }
    }
    try {
      unpadPKCS7(test3)
    } catch {
      case e: Exception ⇒ {
        ans3 = true
      }
    }
    if (ans1 && ans2 && ans3) {
      println("Prob 15: Success")
    } else {
      println("Prob 15: Fail")
    }
  }

  def unpadPKCS7(str: Array[Byte]): Array[Byte] = {
    val pad = str.last
    val padding = str.reverse.slice(0, pad)
    if (padding.size != pad || padding.count(_ != pad) > 0) {
      throw new RuntimeException("bad padding")
    }
    str.dropRight(pad)
  }
}