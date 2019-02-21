/**
  * Created by lilisun on 2/22/19.
  */
import scala.collection.mutable.ArrayBuffer
object No406 {
  def reconstructQueue(people: Array[Array[Int]]): Array[Array[Int]] = {
    val ps = people.sortBy(x => (- x(0), x(1)))
    val res = ArrayBuffer[Array[Int]]()
    ps.foreach(x => res.insert(x(1), x))
    res.toArray
  }
  def main(args: Array[String]): Unit = {
    val res = ArrayBuffer[Array[Int]]()
    val s = Array[Int](0,0)

    val t1 = Array(Array(7,0), Array(4,4), Array(7,1), Array(5,0), Array(6,1), Array(5,2))
    val r1 = reconstructQueue(t1)
    r1.foreach(x => println(x.mkString))
    println(res.size)
    res.insert(0, s)
    println(res.size)
  }
}
