/**
  * Created by lilisun on 2/16/19.
  */
object No922 {
  def sortArrayByParityII(A: Array[Int]): Array[Int] = merge( A.filter(_%2==0) , A.filter(_%2==1))

  def sortArrayByParity_1(A: Array[Int]): Array[Int] = A.filter(_%2==0) ++ A.filter(_%2==1)

  def swapall(A: Array[Int]): Array[Int] = {
    val shift = (A.length / 2 + 1)% 2
    for(x <- 0 until A.length / 2 if x % 2 == 1){
      val temp = A(x)
      A(x) = A(x + A.length / 2 - shift)
      A(x + A.length / 2 - shift) = temp
    }
    A
  }

  def merge(A: Array[Int], B: Array[Int]):Array[Int] = {
    if(A.length != B.length) return Array[Int]()
    var res = Array[Int]()
    for(x <- A.indices) res = res ++ Array(A(x), B(x))
    res
  }

  def sortArrayByParity_2(A:Array[Int]): Array[Int] = {
    var odd_idx = List[Int]()
    var even_idx = List[Int]()
    for( x <- A.indices if x % 2 != A(x) % 2){
      if(x % 2 == 1) odd_idx = x :: odd_idx
      else even_idx = x :: even_idx
    }
    for(x <- odd_idx){
      val temp = A(x)
      A(x) = A(even_idx.last)
      A(even_idx.last) = temp
      even_idx = even_idx.dropRight(1)
    }
    A
  }


  def main(args: Array[String]): Unit = {
    val a = Array( 4, 2,1,6, 5, 7)
    val t = System.currentTimeMillis()% 100
    println(t)


    println(sortArrayByParity_1(a).mkString)
    println(sortArrayByParityII(a).mkString)
  }
}
