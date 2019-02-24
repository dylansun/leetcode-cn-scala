import scala.collection.mutable
object No985 {
  // A is val that can not be changed
  def sumEvenAfterQueries(A: Array[Int], queries: Array[Array[Int]]): Array[Int] =solvesmallA(A, queries)

  def solvesmallA(A: Array[Int], queries: Array[Array[Int]]): Array[Int] =
  {
    val an = mutable.ArrayBuffer[Int]()
    an ++= A
    var res = an.filter(x => x %2 == 0).sum
    (for(q <- queries) yield{
      if(an(q(1)) % 2 == 0){
        if(q(0) % 2 == 0) res += q(0)
        else res -= an(q(1))
      }
      else{
        if( q(0) % 2 != 0) res += an(q(1)) + q(0)
      }
      an(q(1)) += q(0)
      res
    }).array
  }

  // if A is too big, we only need the interface of get A
  // and we dont need to update A in the disk
  def solveBigA(A: Array[Int], queries: Array[Array[Int]]): Array[Int] = {
    val da = mutable.HashMap[Int, Int]()
    var res = A.filter( _ %2 == 0).sum
    (for(q <- queries) yield {
      val delta = da.getOrElse(q(1), 0)
      val ta = delta + A(q(1))
      //
      if(ta % 2 == 0){
        if(q(0) % 2 == 0) res += q(0)
        else res -= ta
      }
      else{
        if( q(0) % 2 != 0) res += ta + q(0)
      }

      //
      da.put(q(1), delta + q(0))
      res
    }).array
  }


  def main(args: Array[String]): Unit = {


  }
}
