/**
  * Created by lilisun on 3/5/19.
  */
object No964 {
  var memo = scala.collection.mutable.HashMap[String, Int]()
  var x = 0
  def leastOpsExpressTarget(x: Int, target: Int): Int = {
    this.x = x
    val ans =  dp(0, target) - 1
    this.memo.clear()
    ans
  }

  def dp(i: Int, target: Int):Int = {
    val code = "" + i + "#" + target
    if(memo.contains(code))
      return memo(code)

    var ans = 0
    if (target == 0) {
      ans = 0
    } else if (target == 1) {
      ans = cost(i)
    } else if (i >= 39) {
      ans = target + 1
    } else {
      val t = target / x
      val r = target % x
      ans = Math.min(r * cost(i) + dp(i+1, t),
        (x-r) * cost(i) + dp(i+1, t+1))
    }

    memo.put(code, ans)
    return ans
  }
  def cost(i: Int): Int = if(i > 0) i else 2
}
