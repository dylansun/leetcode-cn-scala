/**
  * Created by lilisun on 3/5/19.
  */
object No303 {
  class NumArray(_nums: Array[Int]) {

    val cum = Array.ofDim[Int](_nums.length)
    var initial = true
    def sumRange(i: Int, j: Int): Int = {
      if(initial){
        for(i <- cum.indices){
          i match {
            case 0 => cum(0) = _nums(0)
            case _ => cum(i) = cum(i-1) + _nums(i)
          }
        }
        initial = false
      }

      i match {
        case 0 => cum(j)
        case _ => cum(j) - cum(i-1)
      }
    }

  }

}
