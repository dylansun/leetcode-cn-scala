/**
  * Created by lilisun on 6/23/19.
  */
object No800 {
  object Solution {
    val hexTable = Array("0","1","2","3","4","5","6","7","8","9",
      "a", "b", "c", "d", "e", "f").map(x => x + x)
    val intTable = Map('0' -> 0,
      '1' -> 1,
      '2' -> 2,
      '3' -> 3,
      '4' -> 4,
      '5' -> 5,
      '6' -> 6,
      '7' -> 7,
      '8' -> 8,
      '9' -> 9,
      'a' -> 10,
      'b' -> 11,
      'c' -> 12,
      'd' -> 13,
      'e' -> 14,
      'f' -> 15
    )
    def similarRGB(color: String): String = {
      def f(a:Char, b:Char):String = {
        hexTable (
          (0 to 15)
            .toList
            .sortBy{ n => Math.abs((intTable(a) - n ) * 16 +
              intTable(b) - n)}
            .head
          )
      }

      color(0) +
        f(color(1), color(2)) +
        f(color(3), color(4)) +
        f(color(5), color(6))
    }
  }
  val moles = Array(".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--..")
  def uniqueMorseRepresentations(words: Array[String]): Int = {
    words.map(str => str.toCharArray.map{ch => moles(ch - 'a')}.mkString).distinct.length
  }

  def main(args: Array[String]): Unit = {

    val A = Array("gin", "zen", "gig", "msg")
    println(uniqueMorseRepresentations(A))
  }
}
