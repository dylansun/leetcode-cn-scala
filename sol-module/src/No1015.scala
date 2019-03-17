/**
  * Created by lilisun on 3/17/19.
  */
object No1015 {
  def numDupDigitsAtMostN(N: Int): Int = {
    N - nonDup(N)
  }
  def nonDup(n:Int ):Int = nonDup(trans(n))
  def nonDup(l:List[Int] ):Int = l match{
    case Nil => 0
    case h::Nil => h
    case h::t => {
      var sum = 0
      sum += cumProductOfLenN(l.length - 1, Array(9) ++ (1 to 9).toArray.reverse)
      sum +=(l.head - 1) * productOfFirstN(l.length -1, (1 to 9).toArray.reverse)
      solver(l.tail, Set(l.head), sum)
    }
  }
  def solver(l: List[Int], used: Set[Int], res: Int ):Int = l match {
      case Nil => res
      case h::Nil =>((0 to h).toSet -- used).size + res
      case h::t =>{
        val sum = ((0 until h).toSet -- used)
          .toList
          .map(x =>  bitsOfN(t.length, used + x))
          .sum
        if(used.contains(h)) res + sum
        else solver(t, used + h, res + sum)
      }
    }
  def trans(N:Int):List[Int] = N.toString.toCharArray.map(x => x - '0').toList
  def cumProductOfLenN(n:Int,A : Array[Int]) = {
    (1 to n)
      .map(x => productOfFirstN(x, A))
      .sum
  }
  def productOfFirstN(n: Int, A : Array[Int]):Int =  n match {
    case 0 => 0
    case x:Int if x > 10 => 0
    case _ => A.slice(0, n).product

  }
  def bitsOfN(n:Int, used: Set[Int]):Int = productOfFirstN(n, (1 to 10 - used.size).reverse.toArray)
  def ans(N:Int):Int = {
    (1 to N).count(f)
  }
  def f(n:Int): Boolean = {
    val str = n.toString
    str != str.distinct
  }
  def test():Unit = {
    println(2000 - ans(2000), nonDup(2000))
    println(31230 - ans(31230), nonDup(31230))
  }
  def main(args: Array[String]): Unit = {
    test()
  }
}
