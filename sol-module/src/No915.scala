/**
  * Created by lilisun on 3/26/19.
  */
object No915 {
  def partitionDisjoint(A: Array[Int]): Int = {
    (cum(max)(A.toList, Nil).reverse zip cum(min)(A.reverse.toList, Nil).tail).indexWhere(p)+1
  }
  def p(x:(Int, Int)):Boolean = x._1 <= x._2
  def max(x:Int, y:Int):Int = x max y
  def min(x:Int, y:Int):Int = x min y
  def cum(func: (Int, Int)=> Int)(A:List[Int], acc:List[Int]):List[Int] = (A, acc) match {
    case (Nil, Nil) => Nil
    case (h::t, Nil) => cum(func)(t, h::Nil)
    case (h::t, h1::t1) => cum(func)(t, func(h,h1)::h1::t1)
    case (Nil, h::t) => acc
  }

  def main(args: Array[String]): Unit = {
    println(partitionDisjoint(Array(5,0,3,8,6)))
    println(partitionDisjoint(Array(1,1,1,0,6,12)))
  }
}
