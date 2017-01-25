import java.util.Base64
import Util._

object Prob19 {
  def main(args: Array[String]): Unit = {
    val maxciph = strings.sortBy(_.size).last
    val keystream = Array.fill[Byte](maxciph.size)(0)

    //not sure why, but maximising spaces makes the most intelligible plaintexts
    for (i ← 0 until keystream.size) {
      var max = 0
      var char = 0.toByte
      for (c ← 0 to 255) {
        val count = strings.map(xor(keystream.updated(i, c.toByte), _)).map(_.applyOrElse(i, { c: Int ⇒ 0.toByte })).count(_ == ' ')
        if (count > max) {
          max = count
          char = c.toByte
        }
      }
      keystream.update(i, char.toByte)
    }
    //3 contains century
    keystream.update(13, (strings(3)(13) ^ 'n').toByte)
    keystream.update(14, (strings(3)(14) ^ 't').toByte)
    //10 contains companion
    keystream.update(19, (strings(10)(19) ^ 'o').toByte)
    //21 contains beautiful
    keystream.update(21, (strings(21)(21) ^ 'f').toByte)
    //24 contains horse
    keystream.update(24, (strings(24)(24) ^ 'e').toByte)
    //14 contains utterly
    keystream.update(24, (strings(14)(24) ^ 'e').toByte)
    keystream.update(26, (strings(14)(26) ^ 'l').toByte)
    //6 contains lingered
    keystream.update(7, (strings(6)(7) ^ ' ').toByte)
    keystream.update(8, (strings(6)(8) ^ 'l').toByte)
    keystream.update(9, (strings(6)(9) ^ 'i').toByte)
    //7 contains meaningless
    keystream.update(6, (strings(7)(6) ^ ' ').toByte)
    //28 contains sensitive
    keystream.update(5, (strings(28)(5) ^ 'n').toByte)
    //11 contains around
    keystream.update(0, (strings(11)(0) ^ 'a').toByte)
    keystream.update(1, (strings(11)(1) ^ 'r').toByte)
    //24 contains wrong
    keystream.update(28, (strings(32)(28) ^ 'g').toByte)
    //35 contains part
    keystream.update(29, (strings(35)(29) ^ 't').toByte)
    //27 contains end,
    keystream.update(30, (strings(27)(30) ^ 'e').toByte)
    keystream.update(31, (strings(27)(31) ^ 'n').toByte)
    keystream.update(33, (strings(27)(33) ^ ',').toByte)
    //4 contains head
    keystream.update(34, (strings(4)(34) ^ 'a').toByte)
    keystream.update(35, (strings(4)(35) ^ 'd').toByte)
    //37 contains turn,
    keystream.update(32, (strings(37)(32) ^ ' ').toByte)
    keystream.update(36, (strings(37)(36) ^ 'n').toByte)
    keystream.update(37, (strings(37)(37) ^ ',').toByte)

    val plain = getPlain(keystream)
    val ans = "i have met them at close of day coming with vivid faces from counter or desk among grey eighteenth-century houses."
    if (ans == plain.take(114)) {
      println("Prob 19: Success")
    } else {
      println("Prob 19: Fail")
    }
  }

  def getPlain(keystream: Array[Byte]): String = {
    strings.map(c ⇒ xor(keystream, c)).map(toASCII).mkString(" ")
  }

  val globalkey = keygen
  var strings = Array(
    "SSBoYXZlIG1ldCB0aGVtIGF0IGNsb3NlIG9mIGRheQ==",
    "Q29taW5nIHdpdGggdml2aWQgZmFjZXM=",
    "RnJvbSBjb3VudGVyIG9yIGRlc2sgYW1vbmcgZ3JleQ==",
    "RWlnaHRlZW50aC1jZW50dXJ5IGhvdXNlcy4=",
    "SSBoYXZlIHBhc3NlZCB3aXRoIGEgbm9kIG9mIHRoZSBoZWFk",
    "T3IgcG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA==",
    "T3IgaGF2ZSBsaW5nZXJlZCBhd2hpbGUgYW5kIHNhaWQ=",
    "UG9saXRlIG1lYW5pbmdsZXNzIHdvcmRzLA==",
    "QW5kIHRob3VnaHQgYmVmb3JlIEkgaGFkIGRvbmU=",
    "T2YgYSBtb2NraW5nIHRhbGUgb3IgYSBnaWJl",
    "VG8gcGxlYXNlIGEgY29tcGFuaW9u",
    "QXJvdW5kIHRoZSBmaXJlIGF0IHRoZSBjbHViLA==",
    "QmVpbmcgY2VydGFpbiB0aGF0IHRoZXkgYW5kIEk=",
    "QnV0IGxpdmVkIHdoZXJlIG1vdGxleSBpcyB3b3JuOg==",
    "QWxsIGNoYW5nZWQsIGNoYW5nZWQgdXR0ZXJseTo=",
    "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4=",
    "VGhhdCB3b21hbidzIGRheXMgd2VyZSBzcGVudA==",
    "SW4gaWdub3JhbnQgZ29vZCB3aWxsLA==",
    "SGVyIG5pZ2h0cyBpbiBhcmd1bWVudA==",
    "VW50aWwgaGVyIHZvaWNlIGdyZXcgc2hyaWxsLg==",
    "V2hhdCB2b2ljZSBtb3JlIHN3ZWV0IHRoYW4gaGVycw==",
    "V2hlbiB5b3VuZyBhbmQgYmVhdXRpZnVsLA==",
    "U2hlIHJvZGUgdG8gaGFycmllcnM/",
    "VGhpcyBtYW4gaGFkIGtlcHQgYSBzY2hvb2w=",
    "QW5kIHJvZGUgb3VyIHdpbmdlZCBob3JzZS4=",
    "VGhpcyBvdGhlciBoaXMgaGVscGVyIGFuZCBmcmllbmQ=",
    "V2FzIGNvbWluZyBpbnRvIGhpcyBmb3JjZTs=",
    "SGUgbWlnaHQgaGF2ZSB3b24gZmFtZSBpbiB0aGUgZW5kLA==",
    "U28gc2Vuc2l0aXZlIGhpcyBuYXR1cmUgc2VlbWVkLA==",
    "U28gZGFyaW5nIGFuZCBzd2VldCBoaXMgdGhvdWdodC4=",
    "VGhpcyBvdGhlciBtYW4gSSBoYWQgZHJlYW1lZA==",
    "QSBkcnVua2VuLCB2YWluLWdsb3Jpb3VzIGxvdXQu",
    "SGUgaGFkIGRvbmUgbW9zdCBiaXR0ZXIgd3Jvbmc=",
    "VG8gc29tZSB3aG8gYXJlIG5lYXIgbXkgaGVhcnQs",
    "WWV0IEkgbnVtYmVyIGhpbSBpbiB0aGUgc29uZzs=",
    "SGUsIHRvbywgaGFzIHJlc2lnbmVkIGhpcyBwYXJ0",
    "SW4gdGhlIGNhc3VhbCBjb21lZHk7",
    "SGUsIHRvbywgaGFzIGJlZW4gY2hhbmdlZCBpbiBoaXMgdHVybiw=",
    "VHJhbnNmb3JtZWQgdXR0ZXJseTo=",
    "QSB0ZXJyaWJsZSBiZWF1dHkgaXMgYm9ybi4=").map(c ⇒ AESCTRTransform(Base64.getDecoder.decode(c), globalkey, Array.fill[Byte](8)(0)))
}