object No867 {
  def transpose(A: Array[Array[Int]]): Array[Array[Int]] = {
      val n1 = A.length
      val n2 = A(0).length
      val A_t = Array.ofDim[Int](n2,n1)
      for(i <- 0 to n1 -1)
        for(j <- 0 to n2 -1)
          A_t(i)(j) = A(j)(i)

    A_t
  }

}
