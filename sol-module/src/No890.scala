/**
  * Created by lilisun on 4/12/19.
  */
import  scala.language.postfixOps
object No890 {
  def findAndReplacePattern(words: Array[String], pattern: String): List[String] = {
    words filter isMatch(pattern) toList
  }
  def isMatch(pattern:String)(word:String):Boolean = {
    val t = (pattern zip word).distinct.map(_._2)
    val n =  pattern.distinct.length
    t.length ==n && t.distinct.length == n
  }


  def main(args: Array[String]): Unit = {
    val words = Array("abb","mee", "ccc", "bcd")
    val pattern = "abb"
    println(findAndReplacePattern(words, pattern))
  }
}
