/**
  * Created by lilisun on 1/11/19.
  */
object No868 {
  def binaryGap(N: Int): Int = {
    var last = -1
    var ans  = 0
    for ( i <- 0 to 31)
    if (((N >> i) & 1) > 0) {
      if (last >= 0)
        ans = (ans max  i - last)
      last = i
    }

    return ans
  }
}
