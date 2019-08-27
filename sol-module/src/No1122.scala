/**
  *  Relative Sort Array
  */
object No1122 {
  object Solution {
    def relativeSortArray(A: Array[Int], B: Array[Int]): Array[Int] = {
      val t1 = scala.collection.mutable.HashMap[Int, Int]()
      val t2 = scala.collection.mutable.HashMap[Int, Int]()

      B foreach {x => t1.put(x, 0)}
      A foreach {x => if(t1.contains(x)) t1(x) += 1 else t2.put(x, 1+ t2.getOrElse(x, 0))}
      var l = List.empty[Int]
      B foreach {x => l = if(t1.contains(x)) l ++ List.fill(t1(x))(x) else l}
      (l ++ ((A.filterNot(x => t1.contains(x))).toList).sorted).toArray
    }
  }
}
