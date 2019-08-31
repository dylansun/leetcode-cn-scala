/**
  * 498. 对角线遍历
  */
object No498 {
  object Solution {
    def findDiagonalOrder(A: Array[Array[Int]]): Array[Int] = {
      val n = A.length
      if(n == 0) return Array.empty[Int]
      val m = A(0).length
      val fre =Array.fill(m+n-1)(List.empty[Int])

      for{
        i <- 0 until n
        j <- 0 until m
      }{
        fre(i+j) ::= A(i)(j)
      }

      (0 to m+n - 2).toArray flatMap{ i => i % 2 match {
          case 0 => fre (i)
          case 1 => fre(i).reverse
        }
      }
    }
  }
}
