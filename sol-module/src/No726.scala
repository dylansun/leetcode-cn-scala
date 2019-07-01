/**
  * Created by lilisun on 5/11/19.
  */
import scala.language.postfixOps
object No726 {

  case class Elem(l: Map[String, Int]) {
    def +(that: Elem): Elem = Elem((l /: that.l) { case (map, (k, v)) => map + (k -> (v + map.getOrElse(k, 0))) })
    def *(that: Int): Elem = Elem(l map { case (key, v) => (key, that * v) })
  }
  val zero: Elem = Elem(Map.empty[String, Int])
  def f(l: List[Char], acc: List[Elem]): List[Elem] = l match {
      case Nil => acc
      case '(' :: t => f(t, zero :: acc)
      case ')' :: t => f(t, update(acc, zero))
      case h :: t if h.isLetter => f(t.dropWhile(x => x.isLower | x.isDigit), getFirst(l) :: acc)
      case h :: t if h.isDigit => f(t.dropWhile(x => x.isDigit), (acc.head * getInt(l)) :: acc.tail)
    }
  def getInt(l:List[Char]):Int = (l takeWhile(_.isDigit) mkString) toInt
  def getFirst(l:List[Char]):Elem = {
    val part = l.tail.takeWhile {ch => ch.isLower || ch.isDigit}
    val p1 = part takeWhile(_.isLower)
    val p2 = part dropWhile(_.isLower)
    val cnt = p2 match{case Nil => 1 case _ => p2.mkString.toInt}
    Elem(Map((l.head :: p1).mkString -> cnt))
  }
  def update(mem:List[Elem], acc:Elem = zero):List[Elem] = mem match {
    case `zero`::t => acc::t
    case h::t => update(t, h + acc)
  }
  def countOfAtoms(formula: String): String = {
    f(formula.toList, Nil).reduce(_+_).l.toSeq.sortBy(_._1)
      .map{
        case (k, 1) => k
        case (k,v) => k + v.toString
      }
      .mkString
  }
  def main(args: Array[String]): Unit = {
    val s= "K4Mg2Ch222Oh22Ch11(ON(SO3)2)2"
    val l = Elem(Map("Hg" -> 1, "O" -> 199))
    val l2 = Elem(Map("Hg" -> 2, "O" -> 1))
    println(l.l.toSeq.map{case (k,v) => k + v.toString}.mkString)
    println(countOfAtoms(s))
  }
}
