/**
  * Created by lilisun on 3/19/19.
  */
object No946 {
  def validateStackSequences(pushed: Array[Int], popped: Array[Int]): Boolean = {
    if(pushed.isEmpty && popped.isEmpty) true
    else{
      val pos =  pushed.indexOf(popped.head)
      val pre = pushed.slice(0,pos) intersect popped.tail
      val suf = popped.tail intersect pushed.slice(0, pushed.indexOf(popped.head))
      pre.reverse.toList == suf.toList match {
        case true =>validateStackSequences(pushed.slice(0,pos) ++ pushed.slice(pos+1, pushed.length), popped.tail)
        case _ => false
      }
    }
  }

  def validateStackSequences_slow(pushed: Array[Int], popped: Array[Int]): Boolean = {
    popped.indices
      .forall( i => {
        val item = popped(i)
        val pos = pushed.indexOf(item)
        val preItem = pushed.slice(0, pos) intersect popped.slice(i, popped.length)
        val remain = popped.slice(i, popped.length).filter(x => preItem.contains(x))
        remain.reverse.toList == preItem.toList
      })
  }
}
