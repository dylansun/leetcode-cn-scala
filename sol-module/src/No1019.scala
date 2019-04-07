
/**
  * Definition for singly-linked list.
  * class ListNode(var _x: Int = 0) {
  *   var next: ListNode = null
  *   var x: Int = _x
  * }
  */
object No1019 {
  case class Layer(x:Int, curMax:Int, bl:List[Int])
  def nextLargerNodes(head: ListNode): Array[Int] = {
    //solver(List[Layer]())(getNums(head, Nil), Nil).toArray
    var p = head
    var s = List[Int]()
    while(p!=null) {s ::= p._x; p = p.next}
    g(s.reverse)
  }


  def solver(stack:List[Layer])(nums:List[Int], acc:List[Int]):List[Int] = {
    println(s"stack: $stack, nums: $nums, acc: $acc")
    nums match {
      case Nil => acc
      case h::t => solver(update(stack, h, Layer(h,h, Nil))::stack)(t, find(stack)(h)::acc)
    }
  }
  def update(stack:List[Layer], x:Int, acc:Layer):Layer = stack match {
    case Nil => acc
    case h::t =>
      if(x >= h.curMax) Layer(x, acc.curMax, acc.bl)
      else if(x < h.x) update(t, x, Layer(x, h.curMax max acc.curMax, acc.bl :+ h.x))
      else Layer(x, acc.curMax max h.curMax, acc.bl ++ h.bl.filter(_ > x))
  }
  def find(stack:List[Layer])(x:Int):Int = stack match {
    case Nil => 0
    case h::t => if(h.x > x) h.x else if(x >= h.curMax) 0 else h.bl.find(_ > x).getOrElse(0)
  }
  def getNums(head:ListNode, acc:List[Int]):List[Int] = head match {
    case null => acc
    case _=> getNums(head.next, head.x::acc)
  }

  // 找到右边第一个比x大的值
  def g(nums:List[Int]):Array[Int] = {
    val ans = Array.fill(nums.length)(0)
    var stack = List[(Int, Int)]()
    for(x <- nums.zipWithIndex){
      while(stack.nonEmpty && x._1 > stack.head._1){
        ans(stack.head._2) = x._1
        stack = stack.tail
      }
      stack ::= x
    }
    ans
  }

  //找到右边比x大的最小值
  def f(list:List[Int])(listSorted:List[Int]):Array[Int] = {
    var stack = List[Int]()
    var ill = listSorted
    val ans = Array.fill(list.length)(0)
    while(ill.nonEmpty){
      while(stack.nonEmpty && ill.head > stack.head){
        ans(stack.head) = list(ill.head)
        println(s"${ill.head}, ${stack.head}, ${ans(ill.head)}")
        stack = stack.tail
      }
      stack ::= ill.head
      ill = ill.tail
      println("stack",stack)
      println("nums",ill)
    }
    ans
  }
  def main(args: Array[String]): Unit = {
    val l = List(4,3,2,5,1,8,10)
    val ans = solver(List[Layer]())(l.reverse, Nil)
    println(ans)
  }
}
