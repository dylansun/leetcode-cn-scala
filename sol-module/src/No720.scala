/**
  * Created by lilisun on 3/10/19.
  */
object No720 {
  def longestWord(words: Array[String]): String = {
    def f(str: String):Boolean = {
      str.length match {
        case 1 => true //边界处理
        case _ => words.contains(str.dropRight(1)) && f(str.dropRight(1))
      }
    }
    words
      .filter(f)
      .sortBy( x => (-x.length, x))
      .headOption
      .getOrElse("")
  }

  def main(args: Array[String]): Unit = {
    val words = Array("","w","wo","wor","worl","world")
    def f(str: String):Boolean = {
      str.length match {
        case 0 => true
        case _ => words.contains(str.dropRight(1)) && f(str.dropRight(1))
      }
    }
    words.foreach(x => {
      println(x)
      println(x.dropRight(1))
      println(f(x))
    })
  }
}
