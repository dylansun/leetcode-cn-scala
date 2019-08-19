/**
  * Created by lilisun on 7/2/19.
  */
object No440 {
  def findKthNumber(n: Int, k: Int): Int = {
    def f(x:Int, y:Int, acc:Int):Int = {
      if(x <= n) f(10*x, 10*y, acc +((n+1) min y) - x)
      else acc
    }
    def solve(k:Int, acc:Int):Int = k match {
      case 0 => acc
      case _ => f(acc, acc+1, 0) match {
        case count if k >= count =>solve(k-count, acc + 1)
        case count if k < count => solve(k-1, acc * 10)
      }
    }
    solve(k-1, 1)
  }
}
