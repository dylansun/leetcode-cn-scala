/**
  * Distant Barcodes
  */
object No1054 {
  object Solution {
    def rearrangeBarcodes(A: Array[Int]): Array[Int] = {
      def f(A:Array[Int]):List[(Int,Int)] = {
        val table = scala.collection.mutable.HashMap[Int,Int]()
        A.foreach {x => table.put(x, table.getOrElse(x, 0) + 1)}
        table.toList.map {case (x, y) => (x,y)}.sortBy(- _._2)
      }

      var B = f(A)
      val q = Array.fill(B.head._2)(List.empty[Int])
      var h = (0,0)
      while(B.nonEmpty || h._2 != 0){

        for{i <- q.indices}{
          if(h._2 == 0 && B.nonEmpty){
            h = B.head
            B = B.tail
          }
          if(h._2 != 0){
            q(i) ::= h._1
            h = (h._1, h._2 - 1)
          }
        }
      }
      q.flatMap(_.reverse).toArray
    }
  }
}
