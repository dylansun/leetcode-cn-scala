/**
  * Created by lilisun on 6/27/19.
  */
object No243 {
  def shortestDistance(words: Array[String], word1: String, word2: String): Int = {
    def f(i:List[Int],l1:List[Int], l2:List[Int]):Int = i match {
      case Nil =>
        (for{
          x <- l1
          y <- l2
        } yield Math.abs(x - y)).min
      case h::t => words(h) match {
        case `word1` => f(t, h::l1, l2)
        case `word2` => f(t, l1, h::l2)
        case _ => f(t, l1, l2)
      }
    }
    f(words.indices.toList, Nil, Nil)
  }

  def main(args: Array[String]): Unit = {
    val words = Array("a","s","b","t","s")
    println(shortestDistance(words, "s", "t"))
  }
}
