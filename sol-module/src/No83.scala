/**
  * Created by lilisun on 1/10/19.
  */
object No83 {
  def deleteDuplicates(head: ListNode): ListNode = {
    var current = head
    while (current != null && current.next != null) {
      if (current.next.x == current.x) {
        current.next = current.next.next
      } else {
        current = current.next
      }
    }
    return head
  }
}
