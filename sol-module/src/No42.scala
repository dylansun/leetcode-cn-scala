/**
  * Created by lilisun on 3/23/19.
  */
object No42 {
  def trap(height: Array[Int]): Int = trap(0, height.toList, 0)
  def trap(left: Int, height: List[Int], acc:Int):Int = height match {
      case Nil => acc
      case _ => {
        val idx = height.indexWhere(_>= left)
        idx match {
          case -1 => trap(0, height.reverse:::List(left), acc)
          case _ =>trap(height(idx), height.slice(idx+1, height.length) , acc + idx * left - height.slice(0, idx).sum)
        }
      }
    }

  def main(args: Array[String]): Unit = {
    val A = Array(0,1,0,2,1,0,1,3,2,1,2,1)
    println(trap(A))
  }
}
