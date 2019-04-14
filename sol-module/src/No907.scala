/**
  * Created by lilisun on 4/12/19.
  */
import scala.language.postfixOps
object No907 {
  val mod = 1000000007
  case class Interval(l:Int, r:Int){
    def valid:Boolean = l != r
  }
  def sumSubarrayMins(A: Array[Int]): Int = {
    if (A.length == 0 ) 0 else
      f(A) (List(Interval(0, A.length)), 0)
  }

  def f(A:Array[Int])(l:List[Interval], acc:Int):Int = {
    println(acc, l)
    l match {
      case Nil => acc
      case h::t => f(A)(dList(A)(h) ++ t, dAcc(A)(h) + acc)
    }
  }
  def dAcc(A:Array[Int])(x:Interval):Int = {
    val id = A.indexOf(A.slice(x.l,x.r).min, x.l)
    g(A(id),id + 1 - x.l, x.r - id  )
  }
  def dList(A:Array[Int])(x:Interval):List[Interval] = {
    val id = A.indexOf(A.slice(x.l,x.r).min, x.l)
    List(Interval(x.l, id), Interval(id+1, x.r)) filter {x => x.valid}
  }

  def g(x:Int,y:Int, z:Int):Int = (BigInt(x) * BigInt(y) * BigInt(z) % mod) toInt

  def main(args: Array[String]): Unit = {
    sumSubarrayMins(Array(3,1,2,4))
    Array(3,1,2,4).indexOf(1, 0)
  }
}
