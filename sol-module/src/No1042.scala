/**
  * Created by lilisun on 8/22/19.
  *
  * Flower Planting With No Adjacent
  */
object No1042 {
  object Solution {
    def gardenNoAdj(N: Int, paths: Array[Array[Int]]): Array[Int] = {
      val res = Array.fill(N)(0)
      val G = Array.fill(N)(List.empty[Int])
      paths foreach {case Array(x,y) => G(x-1) ::= y-1; G(y-1) ::= x -1}
      for{i <- 0 until N}{
        var set = Set(1,2,3,4)
        for{j <- G(i)} set -= res(j)
        res(i) = set.toList.head
      }
      res
    }
  }
}
