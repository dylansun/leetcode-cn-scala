object No721 {
  def accountsMerge(accounts: List[List[String]]): List[List[String]] = {
    val length = accounts.length

    val parent = (0 until length).toArray

    @annotation.tailrec
    def root(p: Int): Int =
      if (p != parent(p)) {
        parent(p) = parent(parent(p))
        root(parent(p))
      } else p

    def union(p: Int, q: Int): Unit = {
      val rootP = root(p)
      val rootQ = root(q)
      parent(rootP) = rootQ
    }

    /**
      * mailIndexMap mapping mail to index
      * indexNameMap mapping index to name
      */
    val (_, indexNameTable) = accounts.zipWithIndex.foldLeft((Map[String, Int](), Map[Int, String]())) {
      case ((mailIndexMap, indexNameMap), (account, i)) =>
        val m = account.tail.foldLeft(mailIndexMap) { (map, mail) =>
          if (map.contains(mail)) { // two account have common email, we union they together
            union(i, map(mail))
          }
          map + (mail -> i)
        }
        (m, indexNameMap + (i -> account.head))
    }

    val map: Map[Int, List[String]] = accounts.zipWithIndex.foldLeft(Map[Int, List[String]]()) {
      case (m, (account, i)) =>
        val r = root(i)
        m.updated(r, account.tail ::: m.getOrElse(r, List()))
    }.map { case (key, value) => (key, indexNameTable(key) :: value.distinct.sorted) }
    map.values.toList
  }
}
