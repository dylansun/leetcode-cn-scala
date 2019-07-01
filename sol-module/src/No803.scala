import No442.Solution

/**
  * Created by lilisun on 5/26/19.
  */
object No803 {
  // TLE for dfs
  object DFSSolution {
    case class Point(x:Int, y:Int){
      def +(p:Point):Point = Point(x + p.x, y + p.y)
    }
    def dir = List(Point(-1,0), Point(1,0), Point(0,1), Point(0,-1))
    //    0 1 0    0 1
    //    1 x 1(y) 0 1
    //    1 1 1    1 1
    //    if y falls, the points connected to y should also fall
    //    However, as we search y, we also see z1, z2 are connected to y
    //    so they are equal.
    //    when we do the dfs, maintain a visited table
    //    if they fall, change the grid table and record the change number
    //    also fresh the candidate points(Points may fall)
    def hitBricks(grid: Array[Array[Int]], hits: Array[Array[Int]]): Array[Int] = {
      var ans = List.empty[Int]
      def inBound(p:Point):Boolean = {
        grid.indices.contains(p.x) && grid(p.x).indices.contains(p.y)
      }

      def dfs(l:List[Point], path:List[Point] = Nil):List[Point] = l match {
        case Nil => path
        case h::t =>
          if (!inBound(h) || grid(h.x)(h.y) == 0) dfs(t, path)
          else dfs((dir map {delta => delta + h}).filterNot(path.contains) ++ t, h::path)
      }
      def f(l:Array[Point]):Int = {
        val visited = Array.fill(4)(List.empty[Point])
        for{
          i <- 0 to 3
          p = l(i)
          if inBound(p)
        }{
          /*
          for{
              j <- 0 until i
              if !visited(j).contains(p)
          }*/
          if (!visited.slice(0, i).exists(_.contains(p)))
            visited(i) = dfs(List(p)).distinct
        }
        var acc = 0
        visited foreach { points =>
          //println(points)
          if(!points.exists(_.x == 0))
          {
            acc += points.length
            points foreach {case Point(a,b) => grid(a)(b) = 0}
          }
        }
        acc
      }
      hits foreach {case Array(x,y)=>
        //change grid
        grid(x)(y) = 0
        //
        val l = dir map {p => p + Point(x,y)}
        ans ::= f(l.toArray)
      }
      ans.reverse.toArray
    }
  }
  val test = Array(51,83,51,40,51,40,51,40,83,40,83,83,51,40,40,51,51,51,40,40,40,83,51,51,40,51,51,40,40,51,51,40,51,51,51,40,83,40,40,83,51,51,51,40,40,40,51,51,83,83,40,51,51,40,40,40,51,40,83,40,83,40,83,40,51,51,40,51,51,51,51,40,51,83,51,83,51,51,40,51,40,51,40,51,40,40,51,51,51,40,51,83,51,51,51,40,51,51,40,40)
  object Solution {
    def rearrangeBarcodes(A: Array[Int]): Array[Int] = {


      var B = A.groupBy(x => x)
        .values
        .toList
        .map(x => (x.head, x.length))
        .sortBy(x => -x._2)

      val q = Array.fill(B.head._2)(List.empty[Int])
      var h = (0, 0)
      val stream = Stream.from(0, 1).toIterator
      while (B.nonEmpty || h._2 != 0) {
        if (h._2 == 0 && B.nonEmpty) {
          h = B.head
          B = B.tail
        }
        if (h._2 != 0) {
          q(stream.next() % q.length) ::= h._1
          h = (h._1, h._2 - 1)
        }
      }
      q.flatMap(_.reverse)
    }
  }

  def main(args: Array[String]): Unit = {

    println(Solution.rearrangeBarcodes(test).toList)

  }
}
