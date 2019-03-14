/**
  * Created by lilisun on 3/13/19.
  */
object No986 {
  def intervalIntersection(A: Array[Interval], B: Array[Interval]): Array[Interval] = {
    var ans = List[Interval]()
    var i = 0; var j = 0

    while (i < A.length && j < B.length) {
      // Let's check if A[i] intersects B[j].
      // lo - the startpoint of the intersection
      // hi - the endpoint of the intersection
      val lo = A(i).start max B(j).start
      val hi = A(i).end min B(j).end
      if (lo <= hi)
        ans ::= new Interval(lo, hi)

      // Remove the interval with the smallest endpoint
      if (A(i).end < B(j).end)
        i += 1
      else
        j += 1
    }
    ans.toArray.reverse
  }
}
