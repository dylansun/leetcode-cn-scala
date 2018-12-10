/**
  * Created by lilisun on 12/10/18.
  */
object No57 {

  def insert(intervals: List[Interval], newInterval: Interval): List[Interval] = merge(newInterval::intervals)

  def merge(intervals: List[Interval]): List[Interval] = merge_helper(intervals.sortBy(_._start))

  def merge_helper(intervals: List[Interval]): List[Interval] = {
    if(intervals.length == 0) return intervals
    var res = List[Interval](intervals(0))

    for(i <- 1 to intervals.length-1){
      res = res.dropRight(1):::merge_two(res.last, intervals(i))
    }

    res
  }

  def merge_two(i1: Interval, i2: Interval): List[Interval] = {
    if(i1._end >= i2._start) return List[Interval](new Interval(i1._start, i1._end max i2._end))
    List(i1, i2)
  }

  def main(args: Array[String]): Unit = {
    val i1 = new Interval(0,1)
    val i2 = new Interval(3,5)
    val i3 = new Interval(2,4)
    val l1 = List(i1, i2, i3)
    val l2 = l1.sortBy(_._start)
    val l3 = merge(l1)
    println(l3.length)

  }
}
