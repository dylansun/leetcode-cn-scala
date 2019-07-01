/**
  * Created by lilisun on 6/23/19.
  */
object No604 {
  class StringIterator(str: String) {
    var l = f(str.toCharArray.toList, Nil)
    def f(l:List[Char], acc:List[(Char, Int)]):List[(Char, Int)] = l match {
      case a::t => f(t.dropWhile(_.isDigit), (a, t.takeWhile(_.isDigit).mkString.toInt)::acc)
      case  Nil => acc.reverse
    }
    def next(): Char = l match {
      case (x, 1)::t =>
        l = t
        x
      case (x, n)::t =>
        l = (x , n-1)::t
        x
    }

    def hasNext(): Boolean = {
      l.nonEmpty
    }

  }

  /**
    * Your StringIterator object will be instantiated and called as such:
    * var obj = new StringIterator(compressedString)
    * var param_1 = obj.next()
    * var param_2 = obj.hasNext()
    */
  def main(args: Array[String]): Unit = {
    val obj = new StringIterator("L1e2t1C1o1d1e1")
    println(obj.next())
    println(obj.hasNext())
  }
}
