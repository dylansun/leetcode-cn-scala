/**
  * Created by lilisun on 2/19/19.
  */
object No330 {
  def minPatches(nums: Array[Int], n: Int): Int = {
    var cover: Long = 1; var ans = 0; var idx = 0
    while ( cover <= n){
      if(idx >= nums.length || nums(idx) > cover) {println("h");ans += 1; cover = cover * 2}
      else {cover += nums(idx) ;idx +=1; println("j")}
      println(s"cover: $cover, ans: $ans")
    }
    ans
  }

  def main(args: Array[String]): Unit = {
    val a = Array(1, 2, 31, 33)
    println(Int.MaxValue)
    println(minPatches(a,Int.MaxValue))
  }
}
