/**
  * Created by lilisun on 3/18/19.
  */
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable
object No954 {

  def canReorderDoubled(A: Array[Int]): Boolean = {
    A.count(_==0) % 2 == 0 && solver(A.filter(_>0)) && solver(A.filter(_<0).map(x => -x))
  }
  def solver(A:Array[Int]):Boolean = {
    val nc = mutable.HashMap[Int, Int]()
    for(a <- A) nc.put(a, 1+nc.getOrElse(a, 0))
    val keys = nc.keySet.toArray.sorted
    for(n <- keys if nc(n) > 0){
      if(nc.getOrElse(n * 2, 0) < nc(n)) return false
      else nc(n * 2) -= nc(n)
    }
    true
  }
  def canReorderDoubled_2(A: Array[Int]): Boolean = {
    val nc = mutable.HashMap[Int, Int]()
    for(a <- A) nc.put(a, 1+nc.getOrElse(a, 0))
    val keys = nc.keySet.toArray.sorted
    for(key <- keys if nc(key) > 0){
      key match {
        case 0 => if(nc(key) %2 == 1) return false
        case n:Int if n < 0 => {
          if(n %2 != 0 || nc.getOrElse(n / 2, 0) < nc(n)) return false
          else nc(n / 2) -= nc(n)
        }
        case n:Int if n > 0 => {
          if(nc.getOrElse(n * 2, 0) < nc(n)) return false
          else nc(n * 2) -= nc(n)
        }
      }
    }
    true
  }
  def canReorderDoubled_1(A: Array[Int]): Boolean = {
    if(A.count(_==0) % 2 == 1) return false
    val ab = ArrayBuffer[Int]()
    ab ++= A.filterNot(_==0)
    canReorderDoubled(ab)
  }
  def canReorderDoubled(A: ArrayBuffer[Int]): Boolean = {
    //println(A.toList)
    if(A.length == 0) return true
    if(A.max % 2 == 1) return false
    val a =  A.max
    val  b =if(a > 0) a / 2 else a * 2
    val na = A.count(_== a)
    val nb = A.count(_== b)
    if(na <= nb) canReorderDoubled(A -- ArrayBuffer.fill(na)(a)
      -- ArrayBuffer.fill(na)(b))
    else false

  }v
}
