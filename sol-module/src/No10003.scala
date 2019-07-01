/**
  * Created by lilisun on 4/28/19.
  */
object No10003 {
  def numMovesStones(a: Int, b: Int, c: Int): Array[Int] = {
    val l = Array(a,b,c).sorted
    val la = l(1) - l(0) -1
    val lb = l(2) - l(1) - 1
    val min_move = (la, lb) match {
      case (1, _) => 1
      case (_, 1) => 1
      case (_, _) => (la min 1) + (lb min 1)

    }
    Array( min_move,la + lb)
  }
  case class Point(x:Int, y:Int){
    def +(p:Point):Point = Point(x + p.x, y + p.y)
  }
  def colorBorder(grid: Array[Array[Int]], r0: Int, c0: Int, color: Int): Array[Array[Int]] = {
    val old_color = grid(r0)(c0)
    val n = grid.length
    if(n == 0) return Array[Array[Int]]()
    val m = grid(0).length
    var components = List(Point(r0,c0))
    val dir = List(Point(-1,0), Point(1, 0), Point(0,1), Point(0,-1))
    def inBound(p:Point):Boolean = {
      p.x >= 0 && p.y >= 0 && p.x < n && p.y < m
    }
    def isBorder(p:Point):Boolean = {
      (p.x == 0 || p.y == 0 || p.x == n -1 || p.y == m-1) ||
        (List.fill(4)(p) zip dir).map(x => x._1 + x._2).exists(x => grid(x.x)(x.y) != old_color)
    }

    //val visited = Array.fill(n,m)(false)
    //visited(r0)(c0) = true
    def dfs(l:List[Point], acc:List[Point]):List[Point] = l match {
      case Nil => acc
      case h::t => {
        val adj = (List.fill(4)(h) zip dir).map(x => x._1 + x._2)
          .filter(inBound).filter(p => grid(p.x)(p.y) == old_color).filter(x => !acc.contains(x))
        dfs(adj ++ t, acc ++ adj)
      }
    }
    val borders = dfs( List(Point(r0,c0)),  List(Point(r0,c0))).filter(isBorder)
    for(x <- borders){
      grid(x.x)(x.y) = color
    }
    grid
  }
  def maxUncrossedLines(A: Array[Int], B: Array[Int]): Int = {
    val dp = Array.fill(A.length + 1, B.length + 1)(0)
    for{
      i <- 1 to A.length
      j <- 1 to B.length
    }{
      if(A(i-1) == B(j-1)) dp(i)(j) = dp(i-1)(j-1) + 1
      else dp(i)(j) = dp(i-1)(j) max dp(i)(j-1)
    }
    dp(A.length)(B.length)
  }
  //Input: grid = [[1,2,2],[2,3,2]], r0 = 0, c0 = 1, color = 3
  //Output: [[1, 3, 3], [2, 3, 3]]
  //[1,2,2] [1,3,3]
  //[2,3,2] [2,3,3]

  //Input: grid = [[1,1,1],[1,1,1],[1,1,1]], r0 = 1, c0 = 1, color = 2
  //Output: [[2, 2, 2], [2, 1, 2], [2, 2, 2]]
  //[1,1,1]   [2, 2, 2],
  //[1,1,1]   [2, 1, 2]
  //[1,1,1]   [2, 2, 2]
  val dir = List(Point(-1,0), Point(1, 0), Point(0,1), Point(0,-1))
  def inBound(n:Int,m:Int)(p:Point):Boolean = {
    p.x >= 0 && p.y >= 0 && p.x < n && p.y < m
  }
  def isEscapePossible(blocked: Array[Array[Int]], source: Array[Int], target: Array[Int]): Boolean = {
    if(blocked.length == 0 ) return true
    val bp = blocked.map(x => Point(x(0), x(1)))
    val src = Point(source(0), source(1))
    val tg = Point(target(0), target(1))
    val max_block_area = 60 * 60
    val n = 1000000
    val m = 1000000
    var flag = false
    def dfs(l:List[Point], visited:List[Point], target:Point):Boolean = l match {
      case Nil => visited.length > max_block_area
      case h::t => if(h == target) {flag = true; true}
      else if(visited.length > max_block_area) true else {
        val k = (List.fill(4)(h) zip dir).map(x => x._1 + x._2)
          .filter(inBound(n,m)).filter(p => !visited.contains(p)).filter(p => !bp.contains(p))
        dfs(t ++ k, visited ++ k, target)
      }
    }
    println(flag)
    flag || (dfs(List(src), List(src), tg) && dfs(List(tg), List(tg), src))
  }

}
