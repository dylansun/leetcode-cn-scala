/**
  * Created by lilisun on 7/1/19.
  */
object No1062 {
  // 1500 * 1500
  def longestRepeatingSubstring(S: String): Int = {
    (for{
      i <- S.indices
      j <- i+1 to S.length
    } yield S.slice(i,j))
      .groupBy(x => x)
      .values
      .toList
      .filter(_.length > 1)
      .sortBy(x => - x.head.length) match {
      case Nil => 0
      case h::t => h.head.length
    }


  }
}
