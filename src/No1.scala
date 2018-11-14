import scala.collection.mutable

object No1 {
  def twoSum(nums: Array[Int], target: Int): Array[Int] = {
    for(i <- nums.indices){
      val idx = nums.indexWhere({x:Int => target == x + nums(i) })

      if(idx != -1 && idx != i ) {

          return Array(i, idx)
      }

    }
    return Array(-1, -1)
  }

  def twoSum2(nums: Array[Int], target: Int): Array[Int] = {
    val m = mutable.HashMap[Int,Int]()
    for(i <- nums.indices){
      val idx = m.get(target - nums(i))
      if(idx != None && idx.get != i ) {

        return Array(i, idx.get)
      }
      m.put(nums(i), i)
    }
    return Array(-1, -1)
  }


}
