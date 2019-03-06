/**
  * Created by lilisun on 3/6/19.
  */
object No978 {

  def sign(x: Int): Int = {
    if(x > 0) 1
    else if(x < 0) -1
    else 0
  }
  def isTurbulence(A: Array[Int]): List[Boolean] = {
    if(A.length == 1) return Nil
    (for(i <- 1 until A.length -1) yield {
      val l = A(i)-A(i-1)
      val r = A(i)-A(i+1)
      sign(l) * sign(r) > 0
    }).toList // out of bound
  }
  def countConsecutiveTrue(list: List[Boolean]): Int = {
    println(list)
    var ans = 0
    var count = 0
    for(i <- list.indices){
      if(list(i)) count += 1
      else{
        ans = ans max count
        count = 0
      }
    }
    ans max count
  }
  def maxTurbulenceSize(A: Array[Int]): Int = {
    if(A.length == 1 || A.distinct.length == 1) return 1
    countConsecutiveTrue(isTurbulence(A)) + 2
  }

  def main(args: Array[String]): Unit = {
    val t = Array(9,4,2,10,7,8,8,1,9)
    val t1 = Array(4,8,12,16)
    val t2 = Array(0,8,45,88,48,68,28,55,17,24)
  //  println(maxTurbulenceSize(t))
//    println(maxTurbulenceSize(t1))
    println(maxTurbulenceSize(t2))
  }
}
