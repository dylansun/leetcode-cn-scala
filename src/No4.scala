object No4 {
  def findMedianSortedArrays(nums1: Array[Int], nums2: Array[Int]): Double = findMedianSortedArrays((nums1 ++ nums2).sorted)

  def findMedianSortedArrays(nums: Array[Int]): Double = {
    if(nums.length % 2 != 0) return nums(nums.length  / 2 )
    else return (nums(nums.length / 2).toDouble + nums(nums.length / 2 -1).toDouble) / 2.0
  }

  def main(args: Array[String]): Unit = {

    val nums1 = Array(1, 2)
    val nums2 = Array(3,4)
    println(findMedianSortedArrays(nums1, nums2))
  }
}
