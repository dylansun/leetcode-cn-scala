/**
  * Created by lilisun on 3/11/19.
  */
object No706 {
  class MyHashMap() {
    var myhm = Map[Int, Int]()
    def put(key: Int, value: Int) {
      myhm = myhm  + (key -> value)
    }
    def get(key: Int): Int = {
      myhm.getOrElse(key, -1)
    }
    def remove(key: Int) {
     myhm =  myhm - key
    }
  }
}
