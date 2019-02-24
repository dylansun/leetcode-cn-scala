import scala.collection.mutable
object No985 {
  def sumEvenAfterQueries(A: Array[Int], queries: Array[Array[Int]]): Array[Int] = {
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
}
