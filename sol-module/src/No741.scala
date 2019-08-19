/**
  * Created by lilisun on 7/4/19.
  */
object No741 {
  def cherryPickup(grid: Array[Array[Int]]): Int = {
    val n = grid.length
    val dp = Array.fill(n,n,n)(0)
    val mem_flag = Array.fill(n,n,n)(false)
    def solve(r1:Int, c1:Int, c2:Int):Int = {
      val r2 = r1+c1-c2
      if(List(r1,r2,c1,c2).contains(n) || grid(r1)(c1) == -1 || grid(r2)(c2) == -1)
        Int.MinValue
      else if(r1==n-1 && c1 == n-1)
        grid(r1)(c1)
      else if(mem_flag(r1)(c1)(c2))
        dp(r1)(c1)(c2)
      else{
        var ans = grid(r1)(c1) + (if(c1 != c2) grid(r2)(c2) else 0)
        ans += solve(r1, c1+1,c2+1) max solve(r1+1, c1,c2+1) max solve(r1, c1+1, c2) max solve(r1+1,c1,c2)
        mem_flag(r1)(c1)(c2) = true
        dp(r1)(c1)(c2) = ans
        dp(r1)(c1)(c2)
      }
    }
    0 max solve(0,0,0)
  }
}
