/**
  * Created by lilisun on 4/24/19.
  */

object No713 {
  def numSubarrayProductLessThanK(nums: Array[Int], k: Int): Int = {

    // THIS WILL CASE STACKOVERFLOW
    def f(l:List[Int],p0:Int, y:Int, acc:Int):Int = l match {
      case Nil => acc
      case h::t => if(y * nums(h) >= k) g(t,p0,h, y * nums(h), acc)
      else f(t, p0, y * nums(h), acc + h - p0 + 1)
    }


    def g(l:List[Int], p0:Int,bound:Int, y:Int, acc:Int):Int = {
      if(p0 <= bound && y >= k) g(l, p0+1, bound, y / nums(p0), acc)
      else f(l, p0, y, acc + bound-p0 + 1)
    }

    // THIS PASS THE JUDGE
    def f2(l:List[Int],p0:Int, y:Int, acc:Int):Int = l match {
      case Nil => acc
      case h::t => {
        var p = p0
        var ny = y*nums(h)
        while(p <= h && ny >= k){ ny /= nums(p); p += 1}
        f2(t, p,ny, acc + h - p + 1)
      }
    }
    f2(nums.indices.toList, 0,1, 0)
  }

  def main(args: Array[String]): Unit = {
    val A = Array(2,3,1,2)
    val k = 7333
    numSubarrayProductLessThanK(A,k)
  }
}
