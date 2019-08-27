object No29 {
  object Solution {
    def divide(a: Int, b: Int): Int = {
      if(isIllegal(a, b)) Int.MaxValue
      else f(isNeg(a,b))(help(f(a > 0)(a), f(b > 0)(b), 0))
    }
    // a and b < 0 ,
    def help(a:Int, b:Int, acc:Int):Int = if(a>b) acc else help(a - b,b, acc+1)
    def f(p: Boolean)(x:Int):Int = if(p) -x else x
    def isNeg(a:Int, b:Int):Boolean = {
      (a > 0 && b < 0) || (a < 0 && b > 0)
    }
    def isIllegal(a:Int, b:Int):Boolean = {
      a == Int.MinValue && b == -1
    }
  }
}
