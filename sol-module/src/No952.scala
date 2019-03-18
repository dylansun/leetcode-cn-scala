import scala.collection.mutable
object No952 {

  def largestComponentSize(A: Array[Int]): Int = {
    val N = A.length

    // factored[i] = a list of unique prime factors of A[i]
    val factored = Array.fill(N)(List[Int]())
    for (i <- 0 until N) {
      var d = 2
      var x = A(i)
      while (d * d <= x) {
        if (x % d == 0) {
          while (x % d == 0)
            x /= d
          factored(i) ::= d
        }

        d += 1
      }

      if (x > 1 || factored(i).isEmpty)
        factored(i) ::= x
    }

    // primesL : a list of all primes that occur in factored
    val primes = new mutable.HashSet[Int]()
    for (facs <- factored)
      for (x <- facs)
        primes += x

    val primesL = Array.fill(primes.size)(0)
    var t = 0
    for (x <- primes){
      primesL(t) =x
      t += 1
    }

    // primeToIndex.get(v) == i  iff  primes[i] = v
    val primeToIndex =  mutable.HashMap[Int, Int]()
    for (i <- primesL.indices)
    primeToIndex.put(primesL(i), i)

    val dsu = new DSU(primesL.length)
    for (facs<- factored)
    for (x<- facs)
    dsu.union(primeToIndex(facs.head), primeToIndex(x))

    val count = Array.fill(primesL.length)(0)
    for ( facs <- factored)
    count(dsu.find(primeToIndex(facs.head))) += 1
    count.max
  }
  class DSU(n: Int) {
    val parent = (0 until n).toArray
    def find(x: Int):Int = {
      if (parent(x) != x) parent(x) = find(parent(x))
      parent(x)
    }
    def union( x: Int, y:Int):Unit =  {
      parent(find(x)) = find(y)
    }
  }
  def largestComponentSize_1(A: Array[Int]): Int = {
    var graph = Set[Set[Int]]()
    for(a <- A) graph += Set(a)
    for(i <- A.indices)
      for(j <- i+1 until A.length if gcd(A(i), A(j)) > 1)
      {
        //merge two set
        val si = graph.filter(_.contains(A(i))).head
        val sj = graph.filter(_.contains(A(j))).head
        graph = graph - si - sj + (si ++ sj)
      }
    println(graph)
    graph.map(_.size).max
  }
  def gcd(x:Int, y:Int):Int = y match {
    case 0 => x
    case _ => gcd(y, x%y)
  }
}