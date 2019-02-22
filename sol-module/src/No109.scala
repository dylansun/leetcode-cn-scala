object No109 {
  def sortedListToBST(head: ListNode): TreeNode = {
    def partition(): (Option[ListNode], Option[Int], Option[ListNode]) =
      if (head == null) (None, None, None)
      else if (head.next == null) (None, Some(head.x), None)
      else {
        val (l, v, r) = loop(head, head.next)
        if (r == null) (Some(l), Some(v), None) else (Some(l), Some(v), Some(r))
      }

    def loop(back: ListNode, front: ListNode): (ListNode, Int, ListNode) =
      if (front.next == null || front.next.next == null) {
        val next = back.next
        back.next = null
        (head, next.x, next.next)
      } else loop(back.next, front.next.next)

    partition() match {
      case (Some(l), Some(v), Some(r)) =>
        val root = new TreeNode(v)
        root.left = sortedListToBST(l)
        root.right = sortedListToBST(r)
        root
      case (Some(h), Some(v), None) =>
        val root = new TreeNode(v)
        root.left = new TreeNode(h.x)
        root
      case (None, Some(v), None) => new TreeNode(v)
      case _ => null
    }
  }
}
