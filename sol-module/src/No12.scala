/**
  * Created by lilisun on 3/29/19.
  */
object No12 {

  val subers = List(1, 4, 5, 9 , 10, 40, 50, 90, 100, 400,500, 900, 1000).sortBy(x => -x)
  val table = Map(1 -> "I", 4 -> "IV", 5->"V", 9 -> "IX",
    10 -> "X" , 40 -> "XL", 50 -> "L", 90 -> "XC",
    100 -> "C", 400 -> "CD", 500 -> "D", 900 -> "CM", 1000 -> "M")
 /*
 字符          数值
  I             1
  V             5
  X             10
  L             50
  C             100
  D             500
  M             1000
  */
  def intToRoman_1(num: Int): String = num match {
    case x:Int if x >= 1000 => "M" + intToRoman_1(num - 1000)

    case x:Int if x >= 900 => "CM" + intToRoman_1(num - 900)
    case x:Int if x >= 500 => "D"  + intToRoman_1(num - 500)
    case x:Int if x >= 400 => "CD" + intToRoman_1(num - 400)
    case x:Int if x >= 100 => "C"  + intToRoman_1(num - 100)

    case x:Int if x >= 90  => "XC" + intToRoman_1(num - 90)
    case x:Int if x >= 50  => "L" + intToRoman_1(num - 50)
    case x:Int if x >= 40  => "XL" + intToRoman_1(num - 40)
    case x:Int if x >= 10 => "X" + intToRoman_1(num - 10)

    case x:Int if x >= 9  => "IX" + intToRoman_1(num - 9)
    case x:Int if x >= 5  => "V" + intToRoman_1(num - 5)
    case x:Int if x >= 4  => "IV" + intToRoman_1(num - 4)
    case x:Int if x >= 1 => "I" + intToRoman_1(num - 1)
    case _ => ""
  }

  // f(x) = g(cand(x)) + f(x - cand(x)) => f(x, y) = g(y) + f(x - y, new_y)
  // Here func cand called twice!
  def f(cand: Int => Option[Int])(x:Int):String = {
    if(x>0) table(cand(x).get) + f(cand)(x - cand(x).get) else ""
  }
  def suber(x:Int):Option[Int] = subers.find(_ <= x)
  def intToRoman_2(num:Int ):String = f(suber)(num)


  // 16 => 10 5 1
  def frac(x:Int, acc:List[Int] = Nil):List[Int] = suber(x) match {
    case None => acc.reverse
    case Some(y) => frac(x - y, y::acc)
  }

  def intToRoman(num:Int):String = frac(num).map(table).mkString


}
