import scala.collection.mutable

object No996 {
  var graph = mutable.HashMap[Int, List[Int]]()
  var node = mutable.HashMap[Int, Int]()
  def numSquarefulPerms(A: Array[Int]): Int = {
    initial(A)
    var ans = 0
    for(x<- node.keySet){
      ans += dfs(x, A.length-1)
      println(s"x: $x, ans: $ans")
    }
    ans
  }

  def dfs(start: Int, step: Int):Int = {
    var ans = 1
    node(start) -= 1
    //println(s"start: $start, step: $step, node: $node")
    if(step != 0){
      ans = 0
      if(graph.keySet.contains(start)) {
        for (x <- graph(start)) {
          //println(s"inner loop => start: $start, step: $step, node: $node")
          if (node.keySet.contains(x) && node(x) != 0) {
            ans += dfs(x, step - 1)
          }
        }
      }
    }
    node(start) += 1
    ans
  }

  def initial(A: Array[Int]):Unit = {
    graph.clear()
    node.clear()
    for(x <- A)
      for(y <- A){
        val r = Math.sqrt(x + y).toInt

        if( r*r == x + y){
          println(s"x: $x, y: $y")
          if(graph.keySet.contains(x) && !graph(x).contains(y)) {
            graph(x) ::= y
            println(graph)
          }
          else if(!graph.keySet.contains(x)){
            println(s"2 contains: $x $y")
            println(graph)
            graph.put(x, List(y))
            println(graph)
          }
        }
      }
    for(x <- A ) {
      if(node.keySet.contains(x)) node(x) += 1
      else node.put(x, 1)
    }
    println(s"graph: $graph")
  }

  def main(args: Array[String]): Unit = {
    val t = Array(2, 2 ,2)
    val t2 = Array(2,2)
    val t3 = Array(2,2,2,2,2,2,2,2,2,2)
    val t4 = Array(65,44,5,11)
    val t5 = Array(0,0,0,1,1,1)// (1,0,1,0,0,1) (1,0,0,1,0,1) (0,1,0,1,0,1) (1,0,1,0,1,0)
    println(numSquarefulPerms(t5))
    //println(numSquarefulPerms(t2))
    //println(numSquarefulPerms(t3))
    //println(numSquarefulPerms(t4))


  }
}
