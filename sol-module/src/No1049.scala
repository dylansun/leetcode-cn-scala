/**
  * Created by lilisun on 5/19/19.
  */
import scala.language.postfixOps
object No1049 {
  def lastStoneWeightII(stones: Array[Int]): Int = {
    def f (mem:List[Int], l:List[Int]):Int = l match {
      case Nil => mem.min
      case h::t => f(mem flatMap { x => List(Math.abs(x + h), Math.abs(x - h))} distinct, t)
    }
    f(stones.head::Nil, stones.tail.toList)
  }

  def main(args: Array[String]): Unit = {
    val stones = Array(31,33,21, 40, 26)
   // println(lastStoneWeightII(stones))
    val table = scala.collection.mutable.HashMap[String, Int]()
    table.put("kye", 1)
    println(table)
    table -="kye"
    println(table)
  }
}
