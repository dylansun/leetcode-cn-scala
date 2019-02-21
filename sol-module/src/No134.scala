/**
  * Created by lilisun on 2/22/19.
  */
class No134 {
  def canCompleteCircuit(gas: Array[Int], cost: Array[Int]): Int = {
    val gain = for(x <- gas.indices) yield gas(x) - cost(x)
    if(gain.sum < 0) return -1
    var tgain = 0; var start = 0
    for(x <- gain.indices){
      tgain += gain(x)
      if(tgain < 0){
        start = x + 1
        tgain = 0
      }
    }
    start
  }
}
