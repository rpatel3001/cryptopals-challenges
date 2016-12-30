object Prob09 {
	def main(args: Array[String]) = {
		var orig = "YELLOW SUBMARINE".getBytes
		var ans = orig ++ Array.fill[Byte](4)(4)
		var out = padPKCS7(orig, 20)
		if(out.mkString == ans.mkString) {
			println("Prob 09: Success")
		} else {
			println("Prob 09: Fail")
		}
	}

	def padPKCS7(str: Array[Byte], size: Int): Array[Byte] = {
		var pad = size - str.size % size
		str ++ (pad.toChar.toString * pad).getBytes
	}
}
