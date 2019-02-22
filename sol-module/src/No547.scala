object No547 {
  def findCircleNum(M: Array[Array[Int]]): Int =
    if (M.isEmpty) 0
    else {
      val rowLen = M.length
      val colLen = M.head.length

      val parent = (0 until rowLen * colLen).toArray
      var _count = rowLen

      @annotation.tailrec
      def root(p: Int): Int =
        if (p != parent(p)) {
          parent(p) = parent(parent(p)) // path compression by halving
          root(parent(p))
        } else p

      def union(p: Int, q: Int): Unit = {
        val rootP = root(p)
        val rootQ = root(q)
        if (rootP != rootQ) {
          parent(rootP) = rootQ
          _count -= 1
        }
      }

      for {
        row <- 0 until rowLen
        col <- 0 until colLen
        if M(row)(col) == 1
      } union(row, col)

      _count
    }
}
