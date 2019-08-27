/**
  * 1156. Swap For Longest Repeated Character Substring
  */
object No1156 {
  object Solution {
    case class Elem(ch:Char, n:Int)
    def parser(l:List[Char], xs:List[Elem] = Nil):List[Elem] = l match {
      case Nil => xs
      case h::t => xs match {
        case Nil => parser(t, Elem(h,1)::Nil)
        case Elem(y, n)::t2 =>
          if (y == h) parser(t, Elem(y, n+1)::t2)
          else parser(t, Elem(h,1)::xs)
      }
    }

    def maxRepOpt1(text: String): Int = {
      val fre = scala.collection.mutable.HashMap[Char, Int]()
      text foreach {ch => fre.put(ch, fre.getOrElse(ch, 0) + 1)}
      def solve(xs:List[Elem], acc:Int):Int = xs match {
        case Elem(a,n1)::Elem(b,n2)::Elem(c,n3)::t =>
          if(n2 == 1 && a == c) solve(xs.tail, acc max (fre(a) min (n1+n3+1)))
          else solve(xs.tail, acc max (fre(a) min n1+1))
        case Elem(a,n1)::Elem(b,n2)::Nil =>
          acc max (fre(a) min n1+1) max (fre(a) min n2+1)
        case Elem(a,n1)::Nil => acc max n1
        case Nil => acc
      }
      solve(parser(text.toList, Nil), 0)
    }
  }
}
