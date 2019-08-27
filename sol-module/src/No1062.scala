/**
  * Created by lilisun on 7/1/19.
  */
object No1062 {
  object Solution {
    case class P(x:Int, y:Int){
      def +(that:P):P = P(x + that.x, y + that.y)
    }
    val neigh = List(P(0,1), P(0,-1), P(-1,0), P(1,0))
    def maxDistance(grid: Array[Array[Int]]): Int = {
      val n = grid.length
      val m = grid(0).length
      val l = for{
        i <- 0 until n
        j <- 0 until m
        if grid(i)(j) == 1
      } yield P(i,j)

      if(l.isEmpty || l.length == m * n) return -1

      def inBound(p:P):Boolean = {
        grid.indices.contains(p.x) &&
          grid(p.x).indices.contains(p.y)
      }

      def nei(p:P):List[P]= {
        neigh.map(_+p) filter inBound
      }

      def get(p:P):Int = grid(p.x)(p.y)
      def solve(l:Seq[P],v:Set[P],acc:Int):Int = {
        //println(l, v, v.length)
        if(v.size == n * m)  acc
        else {
          val nl = for{
            x <- l
            y <- nei(x)
            if !v.contains(y)
          }yield y
          solve(nl.distinct, v ++ nl,acc + 1)
        }
      }
      solve(l, l.toSet, 0)
    }
  }
  // 1500 * 1500
  def longestRepeatingSubstring(S: String): Int = {
    (for{
      i <- S.indices
      j <- i+1 to S.length
    } yield S.slice(i,j))
      .groupBy(x => x)
      .values
      .toList
      .filter(_.length > 1)
      .sortBy(x => - x.head.length) match {
      case Nil => 0
      case h::t => h.head.length
    }


  }
}
