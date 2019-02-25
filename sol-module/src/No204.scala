/**
  * Created by lilisun on 2/25/19.
  */
object No204 {
  def countPrimes(n: Int): Int = if(n < 2) 0 else countPrimes((2 to n).toList, 0)

  def countPrimes(l:List[Int], acc: Int): Int = l match {
    case Nil => acc
    case _ => countPrimes(l.tail.filter(_ %l.head != 0), acc + 1)
  }
}
