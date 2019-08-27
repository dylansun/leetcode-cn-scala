/**
  * Created by lilisun on 8/21/19.
  */
object No1157 {
  class MajorityChecker(_arr: Array[Int]) {

    val A = _arr
    def count(x:Int):Array[Int] = {
      val B = Array.fill(20001)(0)
      for{i <- 0 to x} B(A(i)) += 1
      B
    }
    val mem = scala.collection.mutable.HashMap[Int, Array[Int]]()
    def query(left: Int, right: Int, threshold: Int): Int = {
      // println(left,right,threshold)
      if(left == right && threshold == 1) return A(left)
      val ls = if(mem.contains (left)) mem(left) else count(left)
      val rs = if(mem.contains(right)) mem(right) else count(right)
      mem.put(left,ls)
      mem.put(right,rs)
      rs(A(left)) += 1
      for{i <- 1 to 20000 if(rs(i) - ls(i) >= threshold)} {rs(A(left)) -=1; return i}
      rs(A(left))-=1
      -1
    }

  }
}
