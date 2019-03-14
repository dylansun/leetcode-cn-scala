/**
  * Created by lilisun on 3/15/19.
  */
import scala.collection.mutable
import util.control.Breaks._

object No882 {
  case class ANode(node: Int, dist: Int)
  implicit  val ord:Ordering[ANode] = Ordering.by(- _.dist)
  def buildGraph(edges: Array[Array[Int]], M: Int, N: Int):  mutable.HashMap[Int, mutable.HashMap[Int, Int]] = {
    val  graph = mutable.HashMap[Int, mutable.HashMap[Int, Int]]()
    for (edge <- edges) {
      val u = edge(0); val v = edge(1); val w = edge(2)
      val mu = graph.getOrElse(u, mutable.HashMap[Int, Int]())
      mu.put(v, w)
      graph.put(u, mu)
      val mv = graph.getOrElse(v, mutable.HashMap[Int, Int]())
      mv.put(u, w)
      graph.put(v, mv)
      //println("---")
      //graph.foreach(println)

    }

    graph
  }
  def reachableNodes(edges: Array[Array[Int]], M: Int, N: Int): Int = {
    val  graph = buildGraph(edges, M, N)

    val pq = mutable.PriorityQueue[ANode]()
    pq.enqueue( ANode(0, 0))

    val dist = mutable.HashMap[Int,Int]()
    dist.put(0, 0)
    val used = mutable.HashMap[Int,Int]()
    var ans = 0

    breakable{
      while (pq.nonEmpty) {
        println(pq)
        val anode = pq.dequeue()
        val node = anode.node
        val d = anode.dist

        if (d <=  dist.getOrElse(node, 0)){
          // Each node is only visited once.  We've reached
          // a node in our original graph.
          ans += 1
          if (graph.contains(node)){
            for (nei <- graph(node).keySet) {
              // M - d is how much further we can walk from this node;
              // weight is how many new nodes there are on this edge.
              // v is the maximum utilization of this edge.
              val weight = graph(node)(nei)
              val  v = weight min (M - d)
              used.put(N * node + nei, v)

              // d2 is the total distance to reach 'nei' (neighbor) node
              // in the original graph.
              val d2 = d + weight + 1
              if (d2 < dist.getOrElse(nei, M+1)) {
                pq.enqueue(ANode(nei, d2))
                dist.put(nei, d2)
              }
            }
          }
        }
      }
    }

    // At the end, each edge (u, v, w) can be used with a maximum
    // of w new nodes: a max of used[u, v] nodes from one side,
    // and used[v, u] nodes from the other.
    // [We use the encoding (u, v) = u * N + v.]
    for (edge <- edges) {
      ans += edge(2) min ( used.getOrElse(edge(0) * N + edge(1), 0) +
                           used.getOrElse(edge(1) * N + edge(0), 0))
    }

    ans
  }

  def main(args: Array[String]): Unit = {
    val edges = Array(Array(0,1,4),Array(1,2,6),Array(0,2,8),Array(1,3,1))
    val M = 10
    val N = 4
    println(reachableNodes(edges, M, N))
    val b = Array(Array(2,4,2),Array(3,4,5),Array(2,3,1),Array(0,2,1),Array(0,3,5))
    println(reachableNodes(b, 14, 5))

  }
}


