/**
  *  Previous Permutation With One Swap
  */
object No1053 {
  object Solution {
    def prevPermOpt1(A: Array[Int]): Array[Int] = {
      var queue = List.empty[Int]
      // 4 6 7
      def f (x:Int, idxs:Array[Int]):Unit = {
        val id = bs(A(x), idxs, 0, idxs.length -1 )
        val tmp = A(x)
        A(x) = A(id)
        A(id) = tmp
      }
      def bs(x:Int, idxs:Array[Int], l:Int, r:Int):Int = {
        if(x > A(idxs.last)) return idxs.last
        if(x < A(idxs.tail.head)) return idxs.head
        val mid = (l + r) >> 1
        println(mid)
        if(A(idxs(mid)) >= x){
          if(A( idxs (mid - 1)) < x) return idxs(mid -1)
          else bs(x, idxs, l, mid - 1)
        }else{
          if(A( idxs(mid + 1)) >= x) return idxs(mid)
          else bs(x, idxs, mid+1, r)
        }
      }
      var found = false
      for {
        i <- A.indices.reverse
      }{
        if (!found && i - 1>= 0 && A(i-1) > A(i) ){
          queue ::= i
          println(queue)
          f(i-1, queue.toArray)
          found = true
        }else{
          queue ::= i
        }
      }
      A
    }
  }
}
