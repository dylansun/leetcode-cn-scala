
object No684 {
  def findRedundantConnection(edges: Array[Array[Int]]): Array[Int] = {
    val length = edges.length
    val parent = (0 to length).toArray

    @annotation.tailrec
    def root(p: Int): Int =
      if (p != parent(p)) {
        parent(p) = parent(parent(p))
        root(parent(p))
      } else p

    def connected(p: Int, q: Int) =
      root(p) == root(q)

    def union(p: Int, q: Int): Boolean = {
      val rootP = root(p)
      val rootQ = root(q)
      if (rootP == rootQ) false
      else {
        parent(rootP) = rootQ
        true
      }
    }

    edges.filterNot {
      case Array(p, q)
      => union(p, q)
    }.last
  }
}
