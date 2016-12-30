import Prob03.toASCII

object Prob08 {
	def main(args: Array[String]): Unit = {
		var lines = io.Source.fromFile("data8.txt").getLines
		var maxdiff = 0
		var ecb = ""
		for(line <- lines) {
			var blocks = line.grouped(16).toList.sorted
			var blockset = blocks.toSet
			var ident = blocks.size - blockset.size
			if(ident > maxdiff) {
				maxdiff = ident
				ecb = line
			}
		}
		if(ecb.slice(0,10) == "d880619740") {
			println("Prob 08: Success")
		} else {
			println("Prob 08: Fail")
		}
	}
}