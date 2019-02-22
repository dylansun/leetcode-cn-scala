/**
  * Created by lilisun on 2/23/19.
  */
object No24 {
  def swapPairs(head: ListNode): ListNode = {
    val sentinel = new ListNode(-1)
    sentinel.next = head

    @annotation.tailrec
    def loop(node: ListNode): Unit = {
      if (node != null && node.next != null && node.next.next != null) {
        val next = node.next
        node.next = next.next
        next.next = next.next.next
        node.next.next = next
        loop(next)
      }
    }
    loop(sentinel)
    sentinel.next
  }
}
