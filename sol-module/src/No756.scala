import scala.collection.mutable
object No756 {
  object Attemp1{
    def pyramidTransition(bottom: String, allowed: List[String]): Boolean = {
      val table = scala.collection.mutable.HashMap[(Char, Char), List[Char]]()
      allowed.foreach{str =>
        val key = (str(0), str(1))
        val value = str(2)
        table.put(key, value::table.getOrElse(key, Nil))
      }
      def g0(l:List[Char]):List[List[Char]] = g(l, Nil)
      def g(l:List[Char], acc:List[List[Char]]):List[List[Char]] = l match {
        case Nil => Nil
        case h::Nil => acc map (_.reverse)
        case h1::h2::t =>
          if(!table.contains((h1, h2))) Nil
          else g(h2::t, acc flatMap {x => table((h1,h2)) map { elem => elem::x}})
      }
      def f(l: List[List[Char]]):Boolean =  l match {
        case Nil => false
        case _ =>
          if(l exists {path => path.length == 1}) true
          else f(l flatMap g0)
      }
      f(List(bottom.toList))
    }
  }
  def pyramidTransition(bottom: String, allowed: List[String]): Boolean = {
    val table = scala.collection.mutable.HashMap[(Char, Char), List[Char]]()
    allowed.foreach{str =>
      val key = (str(0), str(1))
      val value = str(2)
      table.put(key, value::table.getOrElse(key, Nil))
    }
    def infer(l1:List[Char], l2:List[Char]):List[Char] = {
      val ans = for{
        x <- l1
        y <- l2
        if table.contains((x,y))
      } yield table((x,y))
      ans.flatten.distinct
    }
    def g(l:List[List[Char]], acc:List[List[Char]]):List[List[Char]] = l match {
      case Nil => Nil
      case h::Nil => acc.reverse
      case h1::h2::t => g(h2::t, infer(h1,h2)::acc)
    }
    def f(l: List[List[Char]]):Boolean =  l match {
      case Nil => false
      case h::Nil => h.nonEmpty
      case h1::h2::t => f(g(l, Nil))
    }
    f(bottom.toList map {ch => List(ch)})
  }

  def main(args: Array[String]): Unit = {
    pyramidTransition("ABC", List("ABD","BCE","DEF","FFF"))
  }
}
