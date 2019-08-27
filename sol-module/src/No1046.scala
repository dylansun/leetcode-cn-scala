/**
  * Created by lilisun on 8/22/19.
  * 1046. 最后一块石头的重量
  * 1046. Last Stone Weight
  */
object No1046 {
  def lastStoneWeight(A: Array[Int]): Int = {
    def f(l:List[Int]):Int = l match {
      case Nil => 0
      case h::Nil => h
      case h1::h2::t => f(((h1-h2)::t).sortBy(x => -x))
    }

    f(A.sortBy(x => -x).toList)
  }
}
