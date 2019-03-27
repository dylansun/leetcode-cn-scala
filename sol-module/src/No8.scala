/**
  * Created by lilisun on 3/27/19.
  */
object No8 {
  def myAtoi(str: String): Int = {
    solver(rmLeadPlus(parse(str.toList, Nil)))
  }

  def solver(l: List[Char]): Int = {
    if(overflow(l)){
      if(l.head == '-') Int.MinValue
      else Int.MaxValue
    }else{
      myAtoi(l, 0)
    }
  }

  def rmLeadPlus(str:List[Char]):List[Char] = if(str.nonEmpty && str.head == '+') str.tail else str
  // 9 8 7 6 2 -
  def parse(str: List[Char], acc: List[Char]):List[Char] = (str, acc) match {
    case (Nil,  _) => if(acc.count( x => ('0' to '9').contains(x)) == 0) List('0') else acc.reverse
    case (h::t, Nil) =>  ('0' to '9').contains(h) || h == '-' || h == '+' match {
      case true => parse(t, h::Nil)
      case _ => if(h == ' ') parse(t, Nil) else List('0')
    }
    case (h::t, _) => ('0' to '9').contains(h) match {
      case true => parse(t, h::acc)
      case _ => if(acc.count( x => ('0' to '9').contains(x)) == 0) List('0') else acc.reverse
    }
  }

  // asset l in Bound
  def myAtoi(l:List[Char], acc:Int):Int = l match {
    case Nil => acc
    case '-'::t => - 1 * myAtoi(t, acc)
    case h::t => myAtoi(t, acc * 10 + h - '0')
  }

  def help(l1:List[Char], l2:List[Char]): Boolean ={
    l1.reverse.zipAll(l2.reverse, '0', '0').reverse
    .filter(x => x._1 != x._2)
    .map(x => x._1 > x._2)
    .headOption.getOrElse(false)
  }

  def overflow(l:List[Char]):Boolean = l.head match {
    case '-' =>l.tail.length match {
        case x:Int if x < 10 => false
        case _ => help(l.tail, Int.MinValue.toString.tail.toList)
      }
    case _ => l.length match {
        case x:Int if x < 10 => false
        case _ => help(l, Int.MaxValue.toString.toList)
    }
  }

  def main(args: Array[String]): Unit = {
    println(parse("2147483648".toList, Nil))
    println(myAtoi("2147483648"))
  }
}
