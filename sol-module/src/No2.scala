/**
  * Created by lilisun on 1/9/19.
  */
object No2 {
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = addTwoNumbers(l1, l2, 0)


  /**
    * TO DO ........
    * @param l1
    * @param l2
    * @param carry
    * @return
    */
  def addTwoNumbers(l1: ListNode, l2: ListNode, carry: Int): ListNode = {
    if(l1 == null && l2 == null && carry == 0) return null
    if(l1 == null && l2 == null && carry == 1) return new ListNode(carry)


    if(l1 == null && l2 != null && carry == 0) return l2
    if(l1 == null && l2 != null && carry == 1){

      val x_new = (l2.x + carry) % 10
      val carry_new = (l2.x + carry) / 10
      val l_new = new ListNode(x_new)
      l_new.next = addTwoNumbers(null, l2.next, carry_new)

      return l_new
    }
    if(l1 != null && l2 == null && carry == 0) return l1
    if(l1 != null && l2 == null && carry == 1) {
      val x_new = (l1.x + carry ) % 10
      val carry_new = (l1.x + carry) / 10
      val l_new = new ListNode(x_new)
      l_new.next = addTwoNumbers(l1.next, null, carry_new)

      return l_new
    }

    val x_new = (l1.x + l2.x + carry) % 10
    val carry_new = (l1.x + l2.x + carry) / 10
    val l_new = new ListNode(x_new)
    l_new.next = addTwoNumbers(l1.next, l2.next, carry_new)

    return l_new
  }



}
