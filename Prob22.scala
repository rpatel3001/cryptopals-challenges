import Prob21._

object Prob22 {
  def main(args: Array[String]): Unit = {
    for (i ← 1 to 100) {
      val (num, ans, timeReceived) = timeSeedRand
      val out = crackSeed(num, timeReceived)
      if (out != ans) {
        println("Prob 22: Fail")
        return
      }
    }
    println("Prob 22: Success")
  }

  def crackSeed(num: Int, time: Int): Int = {
    for (t ← 0 to 3000) {
      seed(time - t)
      if (num == rand) {
        return time - t
      }
    }
    -1
  }

  def timeSeedRand(): (Int, Int, Int) = {
    val time = (System.currentTimeMillis / 1000).toInt
    val delay1 = (util.Random.nextInt(1000 - 40) + 40)
    val delay2 = (util.Random.nextInt(1000 - 40) + 40)
    seed(time + delay1)
    (rand, time + delay1, time + delay1 + delay2)
  }
}