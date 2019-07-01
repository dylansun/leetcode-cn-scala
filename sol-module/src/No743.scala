import scala.collection.mutable
object No743 {
  def networkDelayTime(times: Array[Array[Int]], N: Int, K: Int): Int = {
    val table = mutable.HashMap[Int, List[Int]]()
    val cost = mutable.HashMap[(Int, Int), Int]()

    times.map(_.toList) foreach {case x::y::z::Nil =>
        table.put(x, y::table.getOrElse(x, Nil))
        cost.put((x,y), z)
    }

    val dp = Array.fill(N+1)(Int.MaxValue)
    dp(0) = 0; dp(K) = 0

    def update(h:Int)(l:List[Int], acc:List[Int]):List[Int] = l match {
      case Nil => acc
      case dst::t =>
        if(dp(dst) > cost((h,dst)) + dp(h)) {
          dp(dst) = cost((h,dst)) + dp(h)
          update(h)(t, dst::acc)
        } else update(h)(t, acc)
    }

    def dfs(l:List[Int]):Unit = l match {
      case Nil => {}
      case h::t => dfs((t ++ update(h)(table.getOrElse(h, Nil), Nil)).distinct)
    }

    dfs(List(K))

    dp.max match {
      case Int.MaxValue => -1
      case x => x
    }
  }

  def main(args: Array[String]): Unit = {
    val times = Array(Array(2,1,1),Array(2,3,10),Array(3,4,1),Array(1,3,1))
    val N = 4
    val K = 2
    println (networkDelayTime(times, N,K))
  }
}
