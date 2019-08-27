object No1129 {
  import scala.collection.mutable
  object Solution {
    def shortestAlternatingPaths(n: Int, c1: Array[Array[Int]], c2: Array[Array[Int]]): Array[Int] = {
      val A = Array.fill(n)(10000)
      val B = Array.fill(n)(10000)
      A(0) = 0
      B(0) = 0
      val t1 = mutable.HashMap[Int, List[Int]]()
      val t2 = mutable.HashMap[Int, List[Int]]()
      c1 foreach {case Array(x,y) => t1.put(x , y::(t1.getOrElse(x, Nil)))}
      c2 foreach {case Array(x,y) => t2.put(x , y::(t2.getOrElse(x, Nil)))}
      var i = 0
      while(
        (A.filterNot(_==10000).max max B.filterNot(_== 10000).max) >= i
      ){
        // update B based on A with value i
        for{k <- 0 until n
            if t2.keySet contains (k)
            if A(k) == i
        }{
          for{
            y <- t2(k)
          }{
            B(y) = B(y) min (i + 1)
          }
        }

        // update A based on B with value i
        for{
          k <- 0 until n
          if t1.keySet contains (k)
          if B(k) == i
        }{
          for{
            y <- t1(k)
          }{
            A(y) = A(y) min (i+1)
          }
        }
        i+= 1
      }
      (A zip B) map {case (x, y) => x min y } map {x => if(x == 10000) -1 else x}
    }
  }
}
