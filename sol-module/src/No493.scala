/**
  * Created by lilisun on 5/7/19.
  */
object No493 {
  object BITSolution {
    def search(BITree: Array[Int], i: Int): Int = {
      def help(idx: Int, sum: Int): Int = {
        if (idx < BITree.length) help(idx + (idx & -idx), sum + BITree(idx))
        else sum
      }
      help(i, 0)
    }

    def insert(BITree: Array[Int], i: Int): Unit = {
      def help(i: Int): Unit = {
        if (i > 0) {
          BITree(i) += 1
          help(i - (i & -i))
        }
      }
      help(i)
    }

    def reversePairs(nums: Array[Int]): Int = {
      var res = 0
      val copy = nums.sorted
      val BITree = Array.ofDim[Int](copy.length + 1)

      for (num <- nums) {
        res += search(BITree, index(copy, 2 * BigInt(num) + 1))
        insert(BITree, index(copy, num))
      }
      res
    }
    def index(arr: Array[Int], value: BigInt): Int = {
      def help(l: Int, r: Int): Int = {
        if (l <= r) {
          val m = l + ((r - l) >> 1)
          if (arr(m) >= value) help(l, m - 1)
          else help(m + 1, r)
        }
        else l + 1
      }
      help(0, arr.length - 1)
    }
  }
  object BSTSolution{
  class Node(v: Int) {
    val value = v
    var nodesize = 1
    var left: Node = null
    var right: Node = null
  }

  def insert(root: Node, x: Int): Unit = {
    if (root.value == x) root.nodesize += 1
    else if (root.value < x) {
      if (root.right == null) {
        root.right = new Node(x)
      } else insert(root.right, x)
    }
    else {
      if (root.left == null) root.left = new Node(x)
      else insert(root.left, x)
    }
  }

  def find(root: Node, x: BigInt, acc: Int = 0): Int = root match {
    case null => acc
    case _ =>
      if (root.value == x) treeSize(root.right) + root.nodesize + acc
      else if (root.value < x) find(root.right, x, acc)
      else find(root.left, x, treeSize(root.right) + root.nodesize + acc)
  }

  def treeSize(root: Node): Int = root match {
    case null => 0
    case _ => root.nodesize + treeSize(root.left) + treeSize(root.right)
  }

  def bst(nums: Array[Int]): Int = {
    if (nums.length == 0) 0
    else {
      val root = new Node(nums(0))
      var ans = 0
      for {
        i <- 1 until nums.length
      } {
        ans += find(root, 2 * BigInt(nums(i)) + 1)
        insert(root, nums(i))
      }
      ans
    }
  }
}
  object BruteForceSolution {
    def bf(nums: Array[Int]): Int = {
      var ans = 0
      for {
        i <- nums.indices
        j <- i + 1 until nums.length
        if nums(i) > BigInt(nums(j)) * 2
      } ans += 1
      ans
    }
  }
  object SortedAndBSSolution{
    def solver(nums:Array[Int]):Int = {
      var A = Array.empty[Int]
      def insert(num:Int):Unit = {
        // A(pos) < num and A(pos+1) > num
        def findPos(l:Int, r:Int):Int = {
          val m = l + ((r - l) >> 1)
          if(A(m) >= num){
            if(A(m-1) < num) m-1
            else findPos(l, m-1)
          }
          else{
            if(A(m+1) > num) m
            else findPos(m+1, r)
          }
        }

        if(A.length == 0) A = A :+ num else {
          if(A.head >= num) A = Array(num) ++ A
          else if(A.last < num) A = A :+ num
          else {
            val pos = findPos(0, A.length)
            A = A.slice(0,pos+1) ++ Array(num) ++ A.slice(pos+1, A.length) // too much time cost
          }
        }
      }
      def search(num:BigInt):Int = {
        def find(l:Int, r:Int):Int = {
           val m = l + ( (r -l) >> 1)
          if(A(m) >= num){
             if(A(m-1) < num) A.length - m
             else find(l, m-1)
           }
          else{
             if(A(m+1) >= num) A.length - m -1
             else find(m+1, r)
           }
        }
        if(A.length == 0 || num > A.last) 0 else if (A.head >= num) A.length else
        find(0, A.length -1)
      }

      var ans = 0
      for{num <- nums}{
        println(A.toList, ans)
        ans += search(2*BigInt(num) + 1)
        insert(num)
      }
      ans
    }
  }
  def reversePairs(nums: Array[Int]): Int = {
    BITSolution.reversePairs(nums)
  }

  def main(args: Array[String]): Unit = {
    val A = Array(3,1,2,3,1,2,4,3,5,1)
    //println(reversePairs(A))
    //println(BruteForceSolution.bf(A))
  }

}
