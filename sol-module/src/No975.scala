/**
  * Created by lilisun on 3/4/19.
  */
import scala.collection.mutable
object No975 {
  case class Jump(idx: Int, parity: Int)
  val mem = mutable.HashMap[Jump, Int]()

  def make(B: Array[Int]): Array[Option[Int]] = {
    val ans: Array[Option[Int]] = Array.ofDim[Option[Int]](B.length).map(x => None)
    var stack = List[Int]()
    for(i <- B){
      while(stack.nonEmpty && i > stack.head){
        ans(stack.head) = Some(i)
        stack = stack.tail
      }
      stack = i::stack
    }
    ans
  }
  def oddEvenJumps(A: Array[Int]): Int = {
    val incA = A.zipWithIndex.sortBy(x => x._1).map(_._2)
    val decA = A.zipWithIndex.sortBy(x => - x._1).map(_._2)
    val oddNext = make(incA)
    val evenNext = make(decA)
    val n = A.length
    val odd = Array.ofDim[Boolean](n)
    val even = Array.ofDim[Boolean](n)
    odd(n-1) = true
    even(n-1) = true
    println(s"odd: ${oddNext.toList}")
    println(s"even: ${evenNext.toList}")
    for(i <- (0 to n-2).toList.reverse){
      println(i)
      if(oddNext(i).nonEmpty) odd(i) = even(oddNext(i).get)
      println(i)
      if(evenNext(i).nonEmpty) even(i) = odd(evenNext(i).get)
    }
    odd.count(_ == true)
  }

  def sf(A:Array[Int]):Int ={
    val ans = if(A.isEmpty) 0 else  (for(i <- A.indices) yield sf(A, i, 1)).sum
    mem.clear()
    ans
  }

  def sf(A:Array[Int], idx: Int, parity:Int ): Int  = {
    val newJump = Jump(idx, parity)
    if(mem.contains(newJump)) mem(newJump)
    if(idx == A.length-1) 1
    else {
      val ans = parity match {
        case 0 => if(evenJump(A, idx).isEmpty) 0 else sf(A, evenJump(A, idx).get, 1)
        case 1 => if(oddJump(A,idx).isEmpty) 0 else sf(A, oddJump(A, idx ).get, 0)
      }
      mem(newJump) = ans
      ans
    }
  }

  def evenJump(A:Array[Int], idx: Int): Option[Int] = {
    val c = A.zipWithIndex.filter(x => x._1 <= A(idx) && x._2 > idx)
    if(c.isEmpty) None else Option(c.sortBy(x => (- x._1, x._2)).array(0)._2)
  }
  def oddJump(A:Array[Int], idx: Int): Option[Int] = {
    val c = A.zipWithIndex.filter(x => x._1 >= A(idx) && x._2 > idx)
    if(c.isEmpty) None else Option(c.sortBy(x => (x._1, x._2)).array(0)._2)
  }


  def main(args: Array[String]): Unit = {
    val A = Array(10,13,14,12,15)

    //println(oddEvenJumps(A))
    //println(A.last)
    //println(oddJump(A, 0))

    val B = Array(2,3,1,1,4)
    //println(evenJump(A, 1))
    //println(sf(B))
    val ans:Array[Option[Int]] = Array.ofDim[Option[Int]](3)
    println(ans.toList)


  }
}
