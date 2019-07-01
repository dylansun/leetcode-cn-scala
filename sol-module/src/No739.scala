/**
  * Created by lilisun on 5/11/19.
  */
object No739 {
  object stackSolution {
    def dailyTemperatures(T: Array[Int]): Array[Int] = {
      val ans = Array.fill(T.length)(0)
      var l = List.empty[Int]
      for{ i <- T.indices}{
        while(l.nonEmpty && T(i) > T(l.head)){
          ans(l.head) = (i-l.head)
          l = l.tail
        }
        l ::= i
      }
      ans
    }
  }
}
