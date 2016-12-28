import Prob3._

object Prob4 {
	def main(args: Array[String]): Unit = {
		var min = (' ', 1000000.0)
		var text = ""
		for (line <- io.Source.fromFile("data4.txt").getLines()) {
			var tmp = decrypt(line)
			if (tmp._2 < min._2) {
				min = tmp
				text = charXOR(toASCII(line), tmp._1)
			}
		}
		print("Prob 4: " + min + " " + text)
	}
}