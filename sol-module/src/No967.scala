/**
  * Created by lilisun on 3/5/19.
  */
object No967 {
  def numsSameConsecDiff(N: Int, K: Int): Array[Int] ={
    var ans = (1 to 9).toSet
    for(_ <- 0 until N-1){
      var ans2 = Set[Int]()
      for( x <- ans){
        val d = x % 10
        if( d - K >= 0)
          ans2 += x * 10 + (d -K)
        if(d + K <= 9)
          ans2 += x * 10 + (d +K)
      }
      ans = ans2
    }
    if(N == 1)
      ans += 0
    ans.toArray
  }

  def numsSameConsecDiff_naive(N: Int, K: Int): Array[Int] = {
    if(N == 1) return (0 to 9).toArray
    (Math.pow(10, N-1).toInt until Math.pow(10, N).toInt).toArray.filter(x => isSameDiff(x,K))
  }

  def isSameDiff(n:Int, K:Int):Boolean = {
    val l = trans(n, Nil)
    (l zip l.tail).forall(x => Math.abs(x._1 - x._2) == K)
  }
  def trans(n:Int, acc: List[Int]):List[Int] = {
    if(n == 0) acc
    else trans(n / 10, (n%10)::acc)
  }

  def main(args: Array[String]): Unit = {
    val n = 123050
    println(trans(n, Nil))
    println(isSameDiff(n, 2))

    val n2 = 1357
    println(isSameDiff(n2, 2))

    println(numsSameConsecDiff(7,5).toList)
  }
}
