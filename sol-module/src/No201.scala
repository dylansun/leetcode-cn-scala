/**
  * Created by lilisun on 6/8/19.
  */
object No201 {
  object Solution {
    def rangeBitwiseAnd(m: Int, n: Int): Int = {
      def f(x:Int,b:Int, cur:Int, zero:Int):Int = b match {
        case 0 => zero
        case _ =>
          if((x & 1) != (b & 1)) f( x>> 1, b >> 1,  cur + 1, cur)
          else f(x >> 1,b >> 1,  cur + 1, zero)
      }

      val shift = f(m & n,n, 1, 0)
      (m & n) >> shift << shift
    }
  }
  object bestSolution{
    def rangeBitwiseAnd(m: Int, n: Int): Int = {
      def f(m:Int, n:Int, offset:Int):Int = {
        if(m == n) m << offset
        else f(m >> 1, n >> 1, offset + 1)
      }
      f(m,n,0)
    }
  }
}
