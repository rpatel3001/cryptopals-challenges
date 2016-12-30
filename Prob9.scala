object Prob9 {
	def main(args: Array[String]) = {
		var orig = "YELLOW SUBMARINE".getBytes
		var ans = orig ++ Array.fill[Byte](4)(4)
		var out = padPKCS7(orig, 20)
		if(out.mkString == ans.mkString) {
			println("Prob 9: Success")
		} else {
			println("Prob 9: Fail")
		}
	}

	def padPKCS7(str: Array[Byte], size: Int): Array[Byte] = {
		var out = new collection.mutable.ArrayBuffer[Byte]
		out ++= str
		val pad = size - str.size
		for(i <- 0 until pad) {
			out += pad.toByte
		}
		out.toArray
	}
}
