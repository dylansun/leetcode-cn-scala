/**
  * Created by lilisun on 2/27/19.
  */
object No25 {
  def reverseKGroup(head: ListNode, k: Int): ListNode = {
    val g = new ListNode(0)
    if(!hasLengthOf(head, k)) {
      head
    } else{
      g.next = reverseFirstKGroup(head, k)
      var p = g.next
      for(x <- 1 until k) p = p.next
      p.next = reverseKGroup(p.next, k)
      g.next
    }

  }

  def reverseFirstKGroup(head: ListNode, k: Int): ListNode = {
    var t = head
    for(x <- 1 until k) t = t.next
    var p = head
    for(x <- 1 until k){
      val tmp = p.next
      p.next = t.next
      t.next = p
      p = tmp
    }
    p
  }

  def hasLengthOf(head: ListNode, len: Int):Boolean =len match {
    case 0 => true
    case _ => head match {
      case null => if (len == 0) true else false
      case _ => hasLengthOf(head.next, len - 1)
    }
  }

  def main(args: Array[String]): Unit = {
    val l1 = new ListNode(1)
    val l2 = new ListNode(2)
    val l3 = new ListNode(3)
    val l4 = new ListNode(4)
    val l5 = new ListNode(5)
    //val l6 = new ListNode(6)

    l1.next = l2
    l2.next = l3
    l3.next = l4
    l4.next = l5
    //l5.next = l6
    println(l1.next)
    var p = l1
    while(p != null) {
      print(s"${p.x} -> ")
      p = p.next
    }
    println()

    p = reverseKGroup(l1, 2)
    while(p != null) {
      print(s"${p.x} -> ")
      p = p.next
    }
  }
}
