/**
  * Created by lilisun on 5/12/19.
  */
object Unknown_02 {
  object Solution {
    case class Point(x:Int, y:Int){
      def +(that:Point):Point = Point(x + that.x, y + that.y)
      def isZero():Boolean = x == 0 && y == 0
    }
    // 0 up
    // 1 right
    // 2 down
    // 3 left
    val d = Array(Point(0,1), Point(1, 0), Point(0, -1), Point(-1, 0))
    def isRobotBounded(str: String): Boolean = (str.count(_=='L') % 4, str.count(_=='R') % 4) match {
      case (x , y) => if (x == y) f(str.toList, Point(0,0), 0) else true
    }

    def f(l:List[Char], pos:Point, dir:Int):Boolean =  l match {
      case Nil => pos.isZero
      case 'L'::t => f(t, pos, (dir + 1) % 4)
      case 'R'::t => f(t, pos, (dir + 3) % 4)
      case 'G'::t => f(t, pos + d(dir), dir)
    }
  }

  def main(args: Array[String]): Unit = {
    println(Solution.isRobotBounded("GGLL"))
  }
}
