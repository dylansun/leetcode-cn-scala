/**
  * Created by lilisun on 11/15/18.
  * Example of class, and simple test
  */
class MedianFinder() {

  /** initialize your data structure here. */
  var data: Array[Int] = Array()

  def addNum(num: Int) {
    if(data.length == 0 )
      data = Array(num)

    else{
      data = addNum(data, num)
    }
  }

  def addNum(data : Array[Int], num: Int): Array[Int] = {
     insertNum(data, num, findPosition_bs(data, num))
  }

  // linear method O(n) -> TLE
  // change to binary search method O(log(n))
  def findPosition_linear(data: Array[Int], num: Int): Int = {
    //special case
    if(num <= data(0)) return -1
    if(num >= data.last) return data.length

    //find position to insert
    for(i <- 0 to data.length -1){
      if(num > data(i) && num <= data(i+1)) return i
    }
    return -10
  }

  def findPosition_bs(data: Array[Int], num: Int): Int = {
    //special case
    if(num <= data(0)) return -1
    if(num >= data.last) return data.length

    //find position to insert

    var l = 0
    var r = data.length - 1

    while(l <= r){
      val mid = (l + r ) / 2
      // l num mid
      if( num == data(mid)){
        return mid
      }
      if(num < data(mid)){
        r = mid - 1
      }
      // mid num r
      else{
        l = mid + 1
      }

    }

    return l


}

  def insertNum(data: Array[Int], num: Int, idx: Int): Array[Int] = {
    (data.slice(0, idx).toList ::: List(num) ::: data.slice(idx, data.length).toList).toArray
  }

  def findMedian(): Double = {
    val n = data.length
    if(n % 2 == 0 ) return (data(n/2).toDouble + data(n/2 -1).toDouble) / 2.0
    else data(n/2)
  }


}

object testFind{
  def main(args: Array[String]): Unit = {
    val k = new MedianFinder()
    k.addNum(6)
    println(k.findMedian())
    k.addNum(10)
    println(k.findMedian())
    k.addNum(2)
    println(k.findMedian())
    k.addNum(6)
    println(k.findMedian())
    k.addNum(5)
    println(k.findMedian())
    k.addNum(0)
    println(k.findMedian())
    k.addNum(6)
    println(k.findMedian())
    k.addNum(3)
    println(k.findMedian())
    k.addNum(1)
    println(k.findMedian())
    k.addNum(0)
    println(k.findMedian())
    k.addNum(0)
    println(k.findMedian())
    println(k.data.mkString)
  }
}
