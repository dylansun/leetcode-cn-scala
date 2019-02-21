/**
  * Created by lilisun on 2/21/19.
  */
object No937 {
  def threeEqualParts(A: Array[Int]): Array[Int] = {
    val no1 = A.count(_ == 1)
    val notExist = Array(-1,-1)
    if(no1 % 3 != 0 || A.length < 3) return notExist
    if(no1 == 0) return Array(0,2)
    val mo1 = for(x <- A.indices if A(x) == 1) yield x
    println(mo1.toVector)
    val idx_1 = mo1(mo1.length / 3 -1)
    val idx_2 = mo1(mo1.length / 3 * 2 -1)
    val idx_3 = mo1.last
    println(s"1: $idx_1, 2: $idx_2, 3: $idx_3")
    val n0 = A.length - 1 - idx_3
    val p1 = idx_1 + n0
    val p2 = idx_2 + n0
    val p = Array(p1, p2+1)
    if(p2 > A.length - 1 ) return notExist
    if(is3p(A, p)) p else notExist
  }


  /**
    * A[0], A[1], ..., A[i] 组成第一部分；
      A[i+1], A[i+2], ..., A[j-1] 作为第二部分；
      A[j], A[j+1], ..., A[A.length - 1] 是第三部分。
      这三个部分所表示的二进制值相等。
    */
  def is3p(A: Array[Int], p: Array[Int]):Boolean = {
    var l1 = List[Int]()
    var l2 = List[Int]()
    var l3 = List[Int]()
    var leading0 = true
    for(x <- 0 to p(0) if A(x) == 1 || (A(x) == 0 && !leading0) ){
      leading0 = false
      l1 = A(x)::l1
      }
    leading0 = true
    for(x <- p(0)+1 until p(1) if A(x) == 1 || (A(x) == 0 && !leading0) ){
      leading0 = false
      l2 = A(x)::l2
    }
    leading0 = true
    for(x <- p(1) until A.length if A(x) == 1 || (A(x) == 0 && !leading0) ){
      leading0 = false
      l3 = A(x)::l3
    }
    l1 == l2 && l2 == l3
  }

  def main(args: Array[String]): Unit = {
    val A = Array(1,0,1,1,1,1,0,1)
    println(threeEqualParts(A).mkString)
  }
}
