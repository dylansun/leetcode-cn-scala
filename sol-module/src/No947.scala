/**
  * Created by lilisun on 3/19/19.
  */
object No947 {

  class DSU(n: Int) {
    val parent = (0 until n).toArray
    def find(x: Int):Int = {
      if (parent(x) != x) parent(x) = find(parent(x))
      parent(x)
    }
    def union( x: Int, y:Int):Unit =  {
      parent(find(x)) = find(y)
    }
  }
  def removeStones(stones: Array[Array[Int]]): Int = {
    if(stones.length == 0) return 0
    val fu = new DSU(stones.length)
    for(i <- stones.indices)
      for(j <- i+1 until stones.length if p(stones(i), stones(j)))
        fu.union(fu.find(i), fu.find(j))
    stones.length - fu.parent.distinct.length
  }
  def removeStones_1(stones: Array[Array[Int]]): Int = {
    if(stones.isEmpty) return 0
    var connect = Set[Set[Int]]()
    for(i <- stones.indices) connect += Set(i)
    for(i <- stones.indices){
      for(j <- i+1 until stones.length){
        if(p(stones(i), stones(j))){
          val si = connect.find(_.contains(i)).get
          val sj = connect.find(_.contains(j)).get
          connect = connect - si - sj + (si ++ sj)
        }
      }
    }
    connect.toArray.map(_.size - 1).sum
  }

  def p(i:Array[Int], j:Array[Int]):Boolean = {
    i(0)==j(0) || i(1)==j(1)
  }
}
