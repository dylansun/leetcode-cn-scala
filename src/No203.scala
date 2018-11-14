/**
  * Created by lilisun on 11/14/18.
  */
object No203 {
  def removeElements(head: ListNode, v: Int): ListNode = {
    if(head == null) return Nil
    if(head.x != v){
      head.next = removeElements(head.next, v)
      return head
    }else{
      return removeElements(head.next, v)
    }
  }
}
