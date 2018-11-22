object No78 {
  def subsets(nums: Array[Int]): List[List[Int]] = {
    if(nums.length == 0) return List[List[Int]]()
    val nums_sorted = nums.sorted

    var ret = List[List[Int]]()
    for(i <- 0 to power(2, nums.length) - 1){
      ret = ret ::: List(sub(nums, i))
    }

    return ret
  }


  def sub(nums: Array[Int], selected: Int): List[Int] = {
    var ret = List[Int]()
    for(i <- 0 to nums.length -1 if isSelected(selected, i)){
      ret = ret ::: List(nums(i))
    }
    ret
  }

  def isSelected(n: Int, k: Int): Boolean = {
    (n >> k) % 2 == 1
  }

  def power(n: Int, k: Int): Int = {
    if(k == 0 ) return 1
    if(k < 0) return 0
    var ans = n
    for(i <- 1 to k-1){
      ans *= n
    }
    ans
  }


  def main(args: Array[String]): Unit = {
    val nums = Array(1, 2 ,4)

    println(isSelected(31, 4))

    println((List(1,3, 5)::: List(6,4 )).mkString)

    println(power(2, 4))

    println(subsets(nums).mkString)

  }
}
