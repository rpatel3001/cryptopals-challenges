import Prob3._
import Prob2._

object Prob5 {
	def main(args: Array[String]): Unit = {
		var str = "Burning 'em, if you ain't quick and nimble\nI go crazy when I hear a cymbal".getBytes
		var out = keyXOR(str, "ICE".getBytes)
		var ans = "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f"
        
        if(out.map(byteToHex).mkString == ans) {
            println("Prob 5: Success")
        } else {
        	println("Prob 5: Fail")
        }
	}
}