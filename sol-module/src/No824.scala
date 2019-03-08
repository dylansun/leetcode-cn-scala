/**
  * Created by lilisun on 3/9/19.
  */
object No824 {
  def toGoatLatin(S: String): String = {
    S
      .split(" ")
      .map(transWord)
      .zipWithIndex
      .map(addTailA)
      .mkString(" ")
  }
  def transWord(x: String): String = {
    if(x matches "[aeiouAEIOU][a-zA-Z]*"){
      x + "ma"
    }else{
      x.tail + x.head + "ma"
    }
  }
  def addTailA(x: (String,Int)):String  = x._1 + List.fill(x._2+1)("a").mkString
  def main(args: Array[String]): Unit = {
    println()
  }
}
