
object No232 {

  class MyQueue() {

    /** Initialize your data structure here. */
    var primary, aux = List.empty[Int]

    /** Push element x to the back of queue. */
    def push(x: Int): Unit = {
      aux = x :: aux
    }

    /** Removes the element from in front of queue and returns that element. */
    def pop(): Int = {
      if (primary.isEmpty)
        move()
      val res = primary.head
      primary = primary.tail
      res
    }

    /** Get the front element. */
    def peek(): Int = {
      if (primary.isEmpty)
        move()
      primary.head
    }

    private[MyQueue] def move(): Unit = {
      primary = aux.reverse
      aux = List.empty[Int]
    }

    /** Returns whether the queue is empty. */
    def empty(): Boolean = primary.isEmpty && aux.isEmpty
  }

}
