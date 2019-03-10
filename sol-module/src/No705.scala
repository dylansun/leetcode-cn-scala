/**
  * Created by lilisun on 3/11/19.
  */
object No705 {

  var my = Set[Int]()
  def add(key: Int) {

    my = my + key
  }

  def remove(key: Int) {
    my = my - key
  }

  def contains(key: Int): Boolean = {
    my.contains(key)
  }
}
