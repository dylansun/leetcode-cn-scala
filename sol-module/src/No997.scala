/**
  * Created by lilisun on 2/24/19.
  */
import scala.collection.mutable
object No997 {
  def findJudge(N: Int, trust: Array[Array[Int]]): Int = {
    val can = mutable.HashSet[Int]()
    (1 to N).foreach(x => can += x)
    //remove people trust others
    trust.foreach(x => if(can.contains(x(0))) can -= x(0))
    if(can.isEmpty) return -1
    val m = mutable.HashMap[Int, mutable.HashSet[Int]]()
    can.foreach(x => m.put(x, mutable.HashSet[Int]()))
    trust.foreach(x => if(m.keySet.contains(x(1))) m(x(1)) += x(0))
    m.foreach(x => if(x._2.size == N - 1) return x._1)
    -1
  }
}
