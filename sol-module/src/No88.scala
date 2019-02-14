/**
  * 给定两个有序整数数组 nums1 和 nums2，将 nums2 合并到 nums1 中，使得 num1 成为一个有序数组。

    说明:

    初始化 nums1 和 nums2 的元素数量分别为 m 和 n。
    你可以假设 nums1 有足够的空间（空间大小大于或等于 m + n）来保存 nums2 中的元素。
    示例:

    输入:
    nums1 = [1,2,3,0,0,0], m = 3
    nums2 = [2,5,6],       n = 3

    输出: [1,2,2,3,5,6]

  */
object No88 {
  def merge(nums1: Array[Int], m: Int, nums2: Array[Int], n: Int): Unit = {
    var count = 0
    var index = 0
    while(count < n){
      if(nums1(index) > nums2(count)){
        for(x <- (index +1 to m + count).reverse ) nums1(x) = nums1(x-1)
        nums1(index) = nums2(count)
        count += 1
      }

      if(index > m+count-1){
        nums1(index) = nums2(count)
        count += 1
      }
      index += 1
    }
  }
  def main(args: Array[String]): Unit = {

    val nums1 = Array(2,0)
    val nums2 = Array(1)
    println("hi")
    merge(nums1,1, nums2,1)
    println("hello")
    println(nums1.mkString)
  }
}
