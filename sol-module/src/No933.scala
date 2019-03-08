/**
  * Created by lilisun on 3/9/19.
  */
object No933 {
  class RecentCounter() {

    var times = List[Int]()
    def ping(t: Int): Int = {
      times ::= t
      count(t, times, 0)
    }

    def count(t: Int,times:List[Int], acc:Int): Int = times match {
      case Nil => acc
      case head::tail => t - head <= 3000 match {
        case true => count(t, tail, acc + 1)
        case fase => acc
      }
    }

  }
}
