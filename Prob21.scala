object Prob21 {
  def main(args: Array[String]): Unit = {
    val rng1 = new MT19937(1)
    val rng2 = new MT19937(1)
    val ans = io.Source.fromFile("data21.txt").getLines.map(c => c.toLong.toBinaryString.takeRight(32))
    if (ans.map(c => c == rng1.rand.toBinaryString.takeRight(32) && c == rng2.rand.toBinaryString.takeRight(32)).count(_ == false) == 0) {
      println("Prob 21: Success")
    } else {
      println("Prob 21: Fail")
    }
  }
}

class MT19937 {
  val N = 624
  val M = 397
  val R = 31
  val A = 0x9908B0DF
  val F = 1812433253
  val U = 11
  val D = 0xFFFFFFFF
  val S = 7
  val B = 0x9D2C5680
  val T = 15
  val C = 0xEFC60000
  val L = 18
  val MASK_UPPER = 1 << R
  val MASK_LOWER = ~MASK_UPPER & D

  var mt = Array.fill[Int](N)(0)
  var index = N + 1

  def this(s: Int) = {
    this
    seed(s)
  }

  def seed(seed: Int): Unit = {
    mt(0) = seed
    for (i ← 1 until N) {
      mt.update(i, (F * (mt(i - 1) ^ (mt(i - 1) >>> 30)) + i) & D)
    }
    index = N
  }

  def rand(): Int = {
    if (index == N + 1) {
      seed(5489)
    }

    if (index == N) {
      for (i ← 0 until N) {
        val x = (mt(i) & MASK_UPPER) | (mt((i + 1) % N) & MASK_LOWER)
        var xA = x >>> 1
        if ((x & 1) == 1) {
          xA ^= A
        }
        mt.update(i, (mt((i + M) % N) ^ xA) & D)
      }
      index = 0;
    }

    var y = mt(index)
    y = y ^ (y >>> U)
    y = y ^ (y << S) & B
    y = y ^ (y << T) & C
    y = y ^ (y >>> L)
    index += 1
    y & D
  }
}