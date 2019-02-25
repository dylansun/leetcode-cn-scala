import scala.collection.mutable
object No566 {
  def matrixReshape(nums: Array[Array[Int]], r: Int, c: Int): Array[Array[Int]] = {
    if(nums.isEmpty || nums(0).isEmpty || nums.length * nums(0).length != r * c) nums
    else{
      val mf = nums.flatten
      val ans = mutable.ArrayBuffer[Array[Int]]()
      for(x <- 0 until r){
        ans += mf.slice(x*c + 0, (x+1)*c )
      }
      ans.toArray
    }

  }
}
