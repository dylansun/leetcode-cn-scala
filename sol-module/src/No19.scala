/**
  * Created by lilisun on 1/10/19.
  */
object No19 {
  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    val dummy = new ListNode(0)
    dummy.next = head
    var first = dummy
    var second = dummy
    // Advances first pointer so that the gap between first and second is n nodes apart
    for (i <- 1 to n + 1) {
      first = first.next
    }
    // Move first to the end, maintaining the gap
    while (first != null) {
      first = first.next
      second = second.next
    }
    second.next = second.next.next
    return dummy.next
  }
}
