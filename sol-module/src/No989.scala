/**
  * Created by lilisun on 2/24/19.
  */
object No989 {
  def addToArrayForm(A: Array[Int], K: Int): List[Int] = {
    var ans = List[Int]()
    var k = K
    val carry = A.reverse.foldLeft(0)((sum, x)=>{
      val t = k % 10 + x + sum
      k = k / 10
      ans :::= List(t % 10)
      t / 10}
    )
    if(k == 0){
      if(carry == 1) ans :::= List(1)
      ans
    }else{
      k = k + carry
      while(k > 0){
        ans :::= List(k % 10)
        k  = k / 10
      }
      ans
    }
  }
}
