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
    val rng = new MT19937
    for (t ← 0 to 3000) {
      rng.seed(time - t)
      if (num == rng.rand) {
        return time - t
      }
    }
    -1
  }

  def timeSeedRand(): (Int, Int, Int) = {
    val rng = new MT19937
    val time = (System.currentTimeMillis / 1000).toInt
    val delay1 = (util.Random.nextInt(1000 - 40) + 40)
    val delay2 = (util.Random.nextInt(1000 - 40) + 40)
    rng.seed(time + delay1)
    (rng.rand, time + delay1, time + delay1 + delay2)
  }
}