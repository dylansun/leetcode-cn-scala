
object No200 {

  def numIslands(grid: Array[Array[Char]]): Int =
    if (grid.isEmpty) 0
    else {
      val rowLen = grid.length
      val colLen = grid.head.length
      var _count = grid.map(row => row.count(_ == '1')).sum

      val parent = (0 until rowLen * colLen).toArray

      def canMoveLeft(i: Int, j: Int) = j - 1 >= 0

      def canMoveRight(i: Int, j: Int) = j + 1 < colLen

      def canMoveUp(i: Int, j: Int) = i - 1 >= 0

      def canMoveDown(i: Int, j: Int) = i + 1 < rowLen

      @annotation.tailrec
      def root(p: Int): Int =
        if (p != parent(p)) {
          parent(p) = parent(parent(p)) // path compression
          root(parent(p))
        }
        else p

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
        if grid(row)(col) == '1'
        p = row * colLen + col
      } {
        if (canMoveLeft(row, col) && grid(row)(col - 1) == '1')
          union(p, row * colLen + (col - 1))
        if (canMoveRight(row, col) && grid(row)(col + 1) == '1')
          union(p, row * colLen + (col + 1))
        if (canMoveUp(row, col) && grid(row - 1)(col) == '1')
          union(p, (row - 1) * colLen + col)
        if (canMoveDown(row, col) && grid(row + 1)(col) == '1')
          union(p, (row + 1) * colLen + col)
      }
      _count
    }
}
