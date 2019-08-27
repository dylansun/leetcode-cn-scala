/**
  * Smallest Sufficient Team
  */
object No1125 {
  import scala.collection.mutable
  object Solution {
    def smallestSufficientTeam(A: Array[String], ll: List[List[String]]): Array[Int] = {
      def f(l:List[String]):Int = {
        var x = 0
        for {
          y <- (A map l.contains).zipWithIndex
          if y._1
        } x += 1 << y._2
        x
      }
      def cond(x:Int)(y:Int):Boolean = (x & y) != 0 && (y & ~(x & y)) == 0
      def g(x:Int):List[Int] = (1 to x).toList.filter {y => cond(x)(y)}
      val table = mutable.HashMap[Int, List[Int]]()
      val track = mutable.HashMap[Int, Int]()
      val path  = mutable.HashMap[Int, (Int, Int)]() // target, (source, choice)
      val l = ll map f
      l.zipWithIndex foreach {case (x, i) => track.put(x,i)}
      l foreach {q => table.put(q, g(q))}

      val dp = Array.fill(1 << A.length)(61)
      dp(0) = 0
      for{ i <- 1 until (1 << A.length)}{
        var c_min = 61
        var from_q = -1
        var from_p = -1
        for{
          q <- l
          k <- table(q)
          p = i & (~k)
        } {
          if (dp(p) + 1 < c_min){
            c_min = dp(p) + 1
            from_p = p
            from_q = q
          }
        }

        if(c_min < dp(i)){
          dp(i) = c_min
          path.put(i, (from_p, track(from_q)))
        }
      }
      def getPath(i:Int, acc:List[Int]):List[Int] = {
        if(i == 0) acc
        else getPath(path(i)._1, path(i)._2 ::acc)
      }
      getPath(dp.indices.last, Nil).toArray
    }
  }
}
