/**
  * Created by lilisun on 4/12/19.
  */
object No895 {
  class FreqStack() {
    val pq = scala.collection.mutable.PriorityQueue[(Int,Int, Int)]()(Ordering.by(x => (x._2, x._3))) // Int, Frec, List of time stamp
    val wc = scala.collection.mutable.HashMap[Int, Int]()
    var time = 0
    def push(x: Int): Unit = {
      wc.put(x, wc.getOrElse(x, 0)+ 1)
      pq.enqueue((x,wc(x), time))
      time += 1
    }
    def pop(): Int = {
      val top = pq.dequeue()._1
      wc(top) -= 1
      top
    }
  }

  def main(args: Array[String]): Unit = {
    val pq = new FreqStack()
    for(x <- List(5,7,5,7,4,5)){
      pq.push(x)
      println(pq.pq.toList)
    }
    for(i <- 1 to 5){
      println(pq.pop())
    }
  }
}
