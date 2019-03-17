/**
  * Created by lilisun on 3/17/19.
  */
object No1014 {
  def shipWithinDays(weights: Array[Int], D: Int): Int = {
    var lo = weights.sum / D max weights.max
    var hi = weights.sum
    var mid = (lo + hi) / 2
    var ans = hi
    while(lo < hi){
      if(can(weights, D, mid)){
        ans = ans min mid
        hi = mid - 1
        mid = (lo + hi) / 2
      }
      else{
        lo = mid + 1
        mid = (lo + hi) / 2
      }
    }
    if(can(weights, D, hi)) ans = ans min hi
    if(can(weights, D, lo)) ans = ans min lo
    println(hi, mid, lo, ans)
    ans
  }



  def can(weights: Array[Int], D: Int, load: Int): Boolean ={
    if(weights.max > load) return false
    var need = 0
    var cur = 0
    for(i <- weights.indices){
      if(cur + weights(i) <= load){
        cur += weights(i)
      }else{
        cur = weights(i)
        need += 1
      }
    }
    println(cur)
    need+1 <= D
  }

  def main(args: Array[String]): Unit = {
    val w = Array(1,2,3,1,1)
    val D = 4
    println(can(w,D, 6))
    println(shipWithinDays(w, D))
  }
}
