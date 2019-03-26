/**
  * Created by lilisun on 3/26/19.
  */
object No891 {
  val mod = 1000000007
  def sumSubseqWidths(A: Array[Int]): Int = {
    solve2(getpow2(A.length,Nil).reverse.toArray)(A.sorted)
  }
  def solve2(table: Array[Int])(sa: Array[Int]):Int = {
    // a0, a1,..., an-1
    // power 0 : (a1-a0), (a2-a1), ..., (an-1 -an-2) => (an-1 - a0)
    // power 1 : (a2-a0), (a3-a1), ..., (an-1 - an-3) => (an-1 + an-2 - a1 - a0
    // power 2 : ... => last 3 - first 3
    // power n-2 : => last (n-1)  - first (n-1)
    cumSolver(table)(sa,cum(sa), cum(sa.reverse))
  }
  def cumSolver(table: Array[Int])(sa: Array[Int], fcum:Array[Int], lcum:Array[Int]):Int = {
    (0 until sa.length-1).toArray
      .map(pn => ((lcum(pn) - fcum(pn))*BigInt(table(pn)) % mod).toInt)
      .foldLeft(0)(modAdd)
  }
  def cum(A:Array[Int]):Array[Int] = if(A.length == 0) Array[Int]()
  else{
    val ans = Array.fill(A.length)(A(0))
    for(i <- 1 until A.length) ans(i) = A(i) + ans(i-1)
    ans
  }
  def modAdd(x:Int, y:Int):Int = (x + y) % mod
  def getpow2(n:Int, list: List[Int]):List[Int] = (n,list) match{
    case (_, Nil )=> getpow2(n-1,List(1))
    case (x:Int, h::t) if x > 0 => getpow2(x-1, (h*2 % mod)::h::t)
    case (0, h::t) => list
  }
  //brute force: O(N2) TLE
  def solve(table: Array[Int])(sa: Array[Int]):Int = {
    sa.indices.flatMap(i=> f(i, sa.length)(i+1, Nil))
      .map(g(sa, table))
      .foldLeft(0)(modAdd)
  }
  def f(i:Int, n:Int)(j:Int, acc:List[(Int, Int)]):List[(Int, Int)] = {
    if(j < n) f(i,n)(j+1, (i,j)::acc) else acc
  }
  def g(A:Array[Int], table:Array[Int])(x:(Int,Int)):Int = { // x = (min,max)
    (table(x._2 - x._1 -1) * BigInt(A(x._2) - A(x._1)) % mod).toInt
  }
  def main(args: Array[String]): Unit = {
    println(sumSubseqWidths(Array(3,7 , 2,3)))

  }
}
