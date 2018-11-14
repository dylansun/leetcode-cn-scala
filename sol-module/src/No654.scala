object No654 {
  def constructMaximumBinaryTree(nums: Array[Int]): TreeNode = {
    //
    if(nums == null) return null

    // find max
    val idx = nums.indexOf(nums.max)
    val head = new TreeNode(nums(idx))

    // divide left and right
    // and check if null
    // left max tree
    if(idx == 0){
      head.left = null
    }else{
      head.left = constructMaximumBinaryTree(nums.slice(0, idx))
    }

    // right max tree
    if(idx == nums.length - 1){
      head.right = null
    }else{
      head.right = constructMaximumBinaryTree(nums.slice(idx + 1, nums.length))
    }

    return head
  }

  def main(args: Array[String]): Unit = {
    val nums = Array(3,2,1,6,0,5)
    val idx = nums.indexOf(nums.max)
    println(nums.slice(0, idx).mkString)
    println(nums.slice(idx+1, nums.length).mkString)

    println(constructMaximumBinaryTree(nums))

  }
}
