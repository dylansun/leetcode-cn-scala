/**
  * Created by lilisun on 2/27/19.
  */
object No844 {
  def backspaceCompare(S: String, T: String): Boolean = {
    def c(s: String, acc: String ): String = s match {
      case "" => acc
      case s: String if s.head == '#' => if(acc != "") c(s.tail, acc.tail) else c(s.tail, acc)
      case _ => c(s.tail, s.head + acc)
    }
    c(S, "") == c(T, "")
  }

  def main(args: Array[String]): Unit = {

    val s = ""
    println(s.tail)
  }
}
