/**
  * Created by lilisun on 11/14/18.
  */
object No21 {
  def mergeTwoLists(l1: ListNode, l2: ListNode): ListNode = {
    if(l1 == null) return l2
    if(l2 == null) return l1
    val head = new ListNode()

    if(l1.x < l2.x){
      head.next = mergeTwoLists(l1.next, l2)
      head.x = l1.x
    }else{
      head.next = mergeTwoLists(l1, l2.next)
      head.x = l2.x
    }

    return head

  }
}
