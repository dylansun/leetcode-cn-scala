import scala.annotation.tailrec

/**
  * Created by lilisun on 2/18/19.
  */
object No23 {
  def mergeKLists_recursive(lists: Array[ListNode]): ListNode = {
    var all_null = true
    lists.foreach( x => if(x != null) all_null = false)
    if(all_null) return null

    val idx = lists.indexOf(lists.minBy(x => if(x == null) Int.MaxValue else x.x))
    val ans = new ListNode(lists(idx).x)
    if(lists(idx).next == null) lists(idx) = null
    else{
      lists(idx).x = lists(idx).next.x
      lists(idx).next = lists(idx).next.next
    }
    ans.next = mergeKLists(lists)
    ans
  }

  def mergeKLists(lists: Array[ListNode]): ListNode = list2listnode(mergeList(lists.map(x => listnode2list(x)).toList))


  def listnode2list(listnode: ListNode): List[Int] = {
    var ans = List[Int]()
    var head = listnode
    while(head != null) {
      ans = head.x :: ans
      head = head.next
    }
    ans
  }


  def mergeList(lists: List[List[Int]]): List[Int] = lists.reduce(_:::_).sorted


  def list2listnode(list: List[Int]): ListNode = {
    if(list.isEmpty) return null
    var p = new ListNode(list.head)
    val head = p
    for(x <- 1 until  list.length){
      val temp = new ListNode(list(x))
      p.next = temp
      p = temp
    }
    head
  }

  def main(args: Array[String]): Unit = {
    val a = new ListNode(1)
    val b = new ListNode(2)
    val c = new ListNode(3)
    c.next = new ListNode(5)
    a.next = new ListNode(6)
    val d = Array[ListNode](a, b,c)
    var k = mergeKLists(d)
    while(k != null){
      print(s"${k.x}->")
      k= k.next
    }
    println()


    val e = d.map(x => listnode2list(x)).toList
    println(e)

    val lists = List(List(1,2,3), List(5,6,9), List(7,8,4))
    println(mergeList(lists))

    val list = List(1,2,3,4,5,8)
    val ln = list2listnode(list)
    var p = ln
    println()
    while(p != null){
      print(s"${p.x} -> ")
      p = p.next
    }


  }
}

/**
  * 输入:
  [
    1->4->5,
    1->3->4,
    2->6
  ]
  输出: 1->1->2->3->4->4->5->6
  */
