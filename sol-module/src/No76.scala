/**
  * Created by lilisun on 3/23/19.
  */
object No76 {
  def minWindow(s: String, t: String): String = {
    var p1 = 0; var p2 = 0
    val dic = t.distinct
    val table = scala.collection.mutable.HashMap[Char, Int]()
    for(i <- dic.indices) table.put(dic(i), i)
    val satis = Array.fill(t.distinct.length)(0)
    for(x <- t) satis(table(x)) += 1
    val state = Array.fill(t.distinct.length)(0)
    var ans = s; var nonans = true
    while(p2 < s.length || isMoveFirst(state, satis)){
      println(p1, p2, state.toList, ans)
      if(isMoveFirst(state, satis)){
        nonans = false
        ans = if(ans.length > p2 - p1) s.slice(p1, p2) else ans
        if(table.contains(s(p1))) state(table(s(p1))) -= 1
        p1 += 1
      }else{
        if(table.contains(s(p2))) state(table(s(p2))) += 1
        p2 += 1
      }
    }

    ans
  }

  def isMoveFirst(state: Array[Int], satis: Array[Int]) = (state zip satis).forall(x  => x._1 >= x._2 )

  def main(args: Array[String]): Unit = {
    val s = "ab"
    val t = "a"
    println(s.length)
    println(minWindow(s,t))
  }
}
