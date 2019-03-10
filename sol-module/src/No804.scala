/**
  * Created by lilisun on 3/9/19.
  */
object No804 {
  val moleDict = Array(".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..","--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-","-.--","--..")
  def uniqueMorseRepresentations(words: Array[String]): Int = {
    words
      .map(_.map(x => moleDict(x-'a')).reduceLeft(_+_))
      .distinct
      .length
  }

  def main(args: Array[String]): Unit = {
    val words = Array("gin", "zen", "gig", "msg")
    println(uniqueMorseRepresentations(words))
  }
}
