/**
  * Created by lilisun on 3/9/19.
  */
object No941 {
  def validMountainArray(A: Array[Int]): Boolean = {
    if(A.length == 0) return false
    val peak = A.indexOf(A.max)
    if(peak == 0 || peak == A.length -1) return false
    for(i <- A.indices.dropRight(1)){
      if(i < peak && A(i) >= A(i+1)) return false
      if(i > peak && A(i) <= A(i+1)) return false
      if(i == peak && A(i) == A(i+1)) return false
    }
    true
  }

  def main(args: Array[String]): Unit = {
    val A = Array(0,1,2,4,2,1)
    println(validMountainArray(A))
  }
}
