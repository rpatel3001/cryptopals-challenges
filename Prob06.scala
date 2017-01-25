import Util._
import java.util.Base64

object Prob06 {
  def main(args: Array[String]): Unit = {
    val str1 = "this is a test".getBytes
    val str2 = "wokka wokka!!!".getBytes
    val out = hamming(str1, str2)
    if (out != 37) {
      println("Prob 06: Fail")
      return
    }

    val base64 = io.Source.fromFile("data6.txt").mkString
    val bytes = Base64.getMimeDecoder().decode(base64)
    val keytext = break(bytes)
    //println("Prob 6: " + toASCII(keytext))

    if (toASCII(keytext) == "Terminator X: Bring the noise") {
      println("Prob 06: Success")
    } else {
      println("Prob 06: Fail")
    }
  }

  def break(bytes: Array[Byte]): Array[Byte] = {
    var editdist = collection.mutable.ListBuffer[(Int, Double)]()
    for (keysize ← 1 to 40) {
      val blocks = collection.mutable.ListBuffer[Array[Byte]]()
      for (i ← 0 until bytes.length / keysize) {
        blocks += bytes.slice(keysize * i, keysize * i + keysize)
      }
      var num = 0
      var sum = 0.0
      for (i ← 0 until blocks.length / 2) {
        num += 1
        sum += hamming(blocks(i * 2), blocks(i * 2 + 1))
      }
      editdist += keysize -> sum / num / keysize
    }
    editdist = editdist.sortBy(_._2).slice(0, 5)

    var keytext = Array[Byte]()
    var text = ""
    var minscore = 100000000000000000000.0
    var ks = 0
    for (key ← editdist) {
      val (t_ks, t_keytext, t_minscore, t_text) = breakXOR(bytes, key._1)
      if (t_minscore < minscore) {
        ks = t_ks
        keytext = t_keytext
        minscore = t_minscore
        text = t_text
      }
    }
    keytext
  }
}