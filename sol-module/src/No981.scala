/**
  * Created by lilisun on 3/6/19.
  */
object No981 {
  class TimeMap() {

    /** Initialize your data structure here. */
    val memo = scala.collection.mutable.HashMap[String, List[(String , Int)]]()

    def set(key: String, value: String, timestamp: Int) {
      memo.put(key, (value, timestamp)::memo.getOrElse(key,Nil))
    }

    def find(list: List[(String, Int)], timeStamp: Int): String = {
      list match {
        case Nil => ""
        case h::t => if(h._2 > timeStamp) find(t, timeStamp) else h._1
      }
    }
    def get(key: String, timestamp: Int): String = {
      val candiate = memo.getOrElse(key, Nil)
      find(candiate, timestamp)
    }

  }
}
