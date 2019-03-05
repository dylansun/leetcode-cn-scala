/**
  * Created by lilisun on 3/5/19.
  */
object No898 {

  //试着减少计算量 pass 80/83
  def subarrayBitwiseORs(A: Array[Int]): Int = {
    if(A.length == 1) return 1
    val dp = Array.ofDim[Int](A.length)

    val mem = scala.collection.mutable.HashSet[Int]()
    var ans = 0
    for(x <- dp.indices)  {
      dp(x) = A(x)
      if(!mem.contains(dp(x))){
        mem += dp(x)
        ans +=1
      }
    }

    for(len <- 2 to A.length){
      //
      for(i <- 0 to A.length - len){
        dp(i) = dp(i) | A(i+len-1)
        if(!mem.contains(dp(i))){
          mem += dp(i)
          ans += 1
        }
      }
    }
    ans
  }
  def subarrayBitwiseORs_dp(A: Array[Int]): Int = {
    if(A.length == 1) return 1
    var dp = Array.ofDim[Int](A.length)
    for(x <- dp.indices)  dp(x) = A(x)
    var ans = Set[Int]()
    ans ++= dp
    for(len <- 2 to A.length){
      val tmp = Array.ofDim[Int](A.length - len + 1)
      //
      for(i <- 0 to A.length - len){
        tmp(i) = dp(i) | A(i+len-1)
      }
      //update dp
      dp = tmp
      ans ++= dp
    }
    ans.size
  }


  def main(args: Array[String]): Unit = {
    //print(2 | 4)

    val ans = Set[Int]()
    val dp = Array(1,2,4)
    print(subarrayBitwiseORs(dp))
  }
}
