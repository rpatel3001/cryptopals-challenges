import Prob3._

object Prob4 {
	def main(args: Array[String]): Unit = {
		var min = (' ', 1000000.0)
		var text = ""
		for (line <- io.Source.fromFile("data4.txt").getLines()) {
			var tmp = decrypt(line)
			if(tmp.length > 0) {
				for(t <- 0 to 0) {
					if (tmp(t)._2 < min._2) {
						min = tmp(t)
						text = charXOR(toASCII(line), tmp(t)._1)
					}
				}
			}
		}
		println(min + " " + text)
	}
}