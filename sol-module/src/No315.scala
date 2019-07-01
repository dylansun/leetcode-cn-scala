/**
  * Created by lilisun on 5/7/19.
  */
object No315 {
  def search(BITree: Array[Int], i: Int): Int = {
    def help(idx: Int, sum: Int): Int = {
      if (idx > 0) help(idx - (idx & -idx), sum + BITree(idx))
      else sum
    }
    help(i min BITree.indices.last, 0)
  }

  def insert(BITree: Array[Int], i: Int): Unit = {
    def help(i: Int): Unit = {
      if (i < BITree.length) {
        BITree(i) += 1
        help(i + (i & -i))
      }
    }
    help(i)
  }
  def index(arr: Array[Int], value: BigInt): Int = {
    def help(l: Int, r: Int): Int = {
      if (l <= r) {
        val m = l + ((r - l) >> 1)
        if (arr(m) > value) help(l, m - 1)
        else help(m + 1, r)
      }
      else l + 1
    }
    help(0, arr.length - 1)
  }
  def countSmaller(nums: Array[Int]): List[Int] = {
    f(nums.reverse)
  }
  def f(nums:Array[Int]):List[Int] = {
    var ans = List.empty[Int]
    val copy = nums.sorted
    val BITree = Array.ofDim[Int](copy.length + 1)
    for{num <- nums}{
      println(s"----------\n$num")
      println("BITree index: ",index(copy, num))
      println("search index:",  index(copy, num-1))
      ans ::= search(BITree, index(copy, num-1))
      insert(BITree, index(copy, num))
      println(BITree.toList)
      println(ans)
    }
    ans
  }

  def main(args: Array[String]): Unit = {
    val nusm = Array(5,2,6,1, 2,3,4,5,3,1,2,1)
    val  nums = Array(26,78,27,100,33,67,90,23,66,5,38,7,35,23,52,22,83,51,98,69,81,32,78,28,94,13,2,97,3,76,99,51,9,21,84,66,65,36,100,41)

    println(countSmaller(nums))
    println(nums.toList.sorted.zipWithIndex)
  }
}
