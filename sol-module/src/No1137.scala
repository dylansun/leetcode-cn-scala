/**
  * 1137. N-th Tribonacci Number
  */
object No1137 {
  object Solution {
    def tribonacci(n: Int): Int = f(0,1,1,n)
    def f(a:Int, b:Int, c:Int, n:Int):Int =  if(n == 0) a  else f(b, c, a+b+c, n-1)
  }
}
