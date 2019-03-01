/**
  * Created by lilisun on 3/2/19.
  */
object No290 {
  case class result(str: String)
  def wordPattern(pattern: String, str: String): Boolean = {
    val words = str.split(" ")

    if(pattern.length != words.length) return false

    val a1 = result((pattern.toCharArray zip words).distinct.map(_._1).mkString)
    val b1 = result(pattern.toCharArray.distinct.mkString)
    val a2 = result((pattern.toCharArray zip words).distinct.map(_._2).mkString)
    val b2 = result(words.distinct.mkString)

    println(a1,str)
    println(b1.str)
    a1 == b1 && a2 == b2
  }
  def main(args: Array[String]): Unit = {
    val str = "dog cat cat dog"
    val pattern = "abba"
    wordPattern(pattern,  str)
  }
}
