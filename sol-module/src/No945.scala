/**
  * Created by lilisun on 3/19/19.
  */
object No945 {
  object firstSolution{
    def minIncrementForUnique(A: Array[Int]): Int = solver(A.sorted)
    // A is sorted in increasing order
    def solver(A:Array[Int]):Int = {
      var acc = 0
      var i = 0
      while(i < A.length){
        var n = 0
        var j = i+1
        while(j < A.length && A(j) == A(i)){n +=1; j+=1}
        if(i+n+1 == A.length) return acc + (n+1)*n / 2
        // (a, a , a, a.., a) => (a+0, a+1, ..., a+n) => (a+0,..,a+k-1, a+k,..,a+k, a+k)
        // b = a + k
        //
        val b = A(i+n+1); val k = b - A(i)
        k > n match {
          case true => {
            i += n +1
            acc += (n+1)*n / 2
          }
          case _ =>{
            for(j <- i+k to i+n) A(j) = b
            i += k
            acc += (k-1)*k/2 + (n-k+1)*k
          }
        }
      }
      acc
    }
  }
  object secondSolution {
    case class WC(n:Int, fre:Int){
      def inc(delta:Int = 1):WC = WC(n, fre+delta)
      def cnt():Int = (fre-1) * fre / 2// sum 0,..., n-1
      def <(wc:WC):Boolean = (n + fre - 1) < wc.n
      def align(wc:WC):Int = {
        val i = wc.n - n
        (i - 1)* i / 2 + i * (fre - i)
      }
      def cal(wc:WC):Int = {
        if(this < wc) this.cnt
        else this align wc
      }
      def add(wc:WC):WC = {
        if(this < wc) wc
        else WC(wc.n, wc.fre + fre - wc.n + n)
      }
    }
    def minIncrementForUnique(A: Array[Int]): Int = {
      solver(f(A.sorted.toList))
    }
    def solver(l:List[WC], acc:Int = 0):Int = l match {
      case Nil => acc
      case h1::Nil => acc + h1.cnt
      case h1::h2::t => solver(h1.add(h2)::t, acc + h1.cal(h2))
    }
    def f(l:List[Int], acc:List[WC] = Nil):List[WC] = (l, acc) match {
      case (Nil, _) => acc.reverse
      case (h::t, Nil) => f(t, WC(h,1)::acc)
      case (h::t, h1::t1) =>
        if(h == h1.n) f(t, h1.inc() :: t1)
        else f(t, WC(h,1)::h1::t1)
    }
  }

  def main(args: Array[String]): Unit = {
    val test = (1 to 100000).toList.reverse.toArray
    val t1 = System.currentTimeMillis()
    println(secondSolution.minIncrementForUnique(Array(1,1,2,2,3,7))) // 50000
    val t2 = System.currentTimeMillis()
    println(s"${t2-t1}")
    val a = Array(1,1,2,2,3,7)
    val b = a.groupBy(x => x)
    println (b.toList map {case (k, v) => (k, v.length)} sortBy {case (k, v) => k} )
  }
}
