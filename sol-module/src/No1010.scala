/**
  * Created by lilisun on 3/17/19.
  */
object No1010 {
  def numPairsDivisibleBy60(time: Array[Int]): Int = {
    var ans = 0
    for(i <- time.indices){
      for(j <- i+1 until time.length ){
        if((time(i) + time(j)) % 60 == 0)
          ans += 1
      }
    }
    ans
  }
}
