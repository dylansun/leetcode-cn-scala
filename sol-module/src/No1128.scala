/**
  * Number of Equivalent Domino Pairs
  */
object No1128 {
  object Solution {
    def numEquivDominoPairs(A: Array[Array[Int]]): Int = {
      A.groupBy {case Array(x,y) => (x min y, x max y)}
        .values.toList
        .foldLeft(0){(sum , x) => sum + x.length * (x.length -1) / 2}
    }
  }

}
