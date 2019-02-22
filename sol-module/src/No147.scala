
object No147 {
  def insertionSortList(head: ListNode): ListNode =
    if (head == null) head
    else {
      var h = head.next
      var res = head
      res.next = null
      while (h != null) {
        val p = h
        h = h.next
        if (p.x < res.x) {
          p.next = res
          res = p
        } else {
          var temp = res
          while (temp.next != null && temp.next.x < p.x)
            temp = temp.next
          p.next = temp.next
          temp.next = p
        }
      }
      res
    }
}
