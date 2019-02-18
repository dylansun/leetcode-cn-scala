/**
  * Created by lilisun on 2/16/19.
  */
object No969 {
  def pancakeSort(A: Array[Int]): List[Int] = pancakeSort(A.toList, false)
  def pancakeSort(A: List[Int], debug:Boolean = true): List[Int] = {
    if(A.isEmpty) return List[Int]()
    var ans = List[Int]()
    //find the max
    val max_idx = A.indexWhere(_ == A.max)
    if(debug) println(s"max idx: $max_idx, list: $A")

    //
    if(max_idx == 0) {
      ans = A.length :: ans
      val A_new = A.reverse
      ans = ans ::: pancakeSort(A_new.dropRight(1), debug)
    }
    else {
      //put the max the first
      val A_pre = A.slice(0, max_idx + 1).reverse
      val A_left = A.slice(max_idx + 1, A.length)
      if(debug){
        println(s"A pre: $A_pre;  A left: $A_left")
      }


      //put the max the end
      val A_new = A_pre ::: A_left
      if(debug) println(s"A new : $A_new")
      ans = List(max_idx + 1 ,A_new.length) ::: ans ::: pancakeSort(A_new.reverse.dropRight(1), debug)

    }
    ans
  }

  def main(args: Array[String]): Unit = {
    val A = Array(3,2,4,1)
    println(pancakeSort(A))
  }
}

/**
  * 969. 煎饼排序
    题目描述
    评论 (11)
    官方题解
    提交记录
    给定数组 A，我们可以对其进行煎饼翻转：我们选择一些正整数 k <= A.length，然后反转 A 的前 k 个元素的顺序。我们要执行零次或多次煎饼翻转（按顺序一次接一次地进行）以完成对数组 A 的排序。

    返回能使 A 排序的煎饼翻转操作所对应的 k 值序列。任何将数组排序且翻转次数在 10 * A.length 范围内的有效答案都将被判断为正确。



    示例 1：

    输入：[3,2,4,1]
    输出：[4,2,4,3]
    解释：
    我们执行 4 次煎饼翻转，k 值分别为 4，2，4，和 3。
    初始状态 A = [3, 2, 4, 1]
    第一次翻转后 (k=4): A = [1, 4, 2, 3]
    第二次翻转后 (k=2): A = [4, 1, 2, 3]
    第三次翻转后 (k=4): A = [3, 2, 1, 4]
    第四次翻转后 (k=3): A = [1, 2, 3, 4]，此时已完成排序。
    示例 2：

    输入：[1,2,3]
    输出：[]
    解释：
    输入已经排序，因此不需要翻转任何内容。
    请注意，其他可能的答案，如[3，3]，也将被接受。
  */
