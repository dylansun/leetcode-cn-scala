/**
  * Created by lilisun on 2/23/19.
  */
object No61 {
  def rotateRight(head: ListNode, k: Int): ListNode = if (k == 0 || head == null || head.next == null) head else {
    var len = 1

    @annotation.tailrec
    def findKthNode(head: ListNode, k: Int): (Int, ListNode) =
      if (head.next == null || k == 0) (k, head)
      else {
        len += 1
        findKthNode(head.next, k - 1)
      }

    @annotation.tailrec
    def loop(front: ListNode, back: ListNode): (ListNode, ListNode) =
      if (front.next == null) (front, back)
      else loop(front.next, back.next)

    val (remaining, node) = findKthNode(head, k)
    if (remaining == 1) { // k is less than or equal to length of list
      if (node.next == null) head
      else {
        val (front, back) = loop(node, head)
        val result = back.next
        back.next = null
        front.next = head
        result
      }
    } else if (k % len == 0) head else { // k is greater than length of list
    val offset = k % len
      var t = head
      for (_ <- 0 until offset)
        t = t.next
      val (front, back) = loop(t, head)
      val result = back.next
      back.next = null
      front.next = head
      result
    }
  }
}
