/**
  * Created by lilisun on 5/6/19.
  */
object No514 {
    def findRotateSteps(ring: String, key: String): Int = {
      val n1 = ring.length
      val n2 = key.length
      val dp = Array.fill(n1,n2)(ring.length * key.length)
      for{i <- 0 until n1 if ring(i)  == key(0)}{
        dp(i)(0) = (i min (n1 - i)) + 1
      }
      for{
        j <- 1 until n2
        i <- 0 until n1
        if key(j) == ring(i)
      }{
        for{pos <- 0 until n1}{
          val step = (i - pos + n1) % n1
          dp(i)(j) = dp(i)(j) min (dp(pos)(j-1) + ( step min (n1 - step)) + 1)

        }
      }
      (0 until n1).map(x => dp(x)(n2-1)).min
  }
}
