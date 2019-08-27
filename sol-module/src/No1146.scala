/**
  * Created by lilisun on 8/21/19.
  */
object No1146 {
  class SnapshotArray(_length: Int) {
    var snap_id = 0
    val table = scala.collection.mutable.HashMap[(Int, Int), Int]() // (Snap_id, index) => value
    def set(index: Int, x: Int) {
      table.put((snap_id, index), x)
    }
    def snap(): Int = {
      snap_id += 1
      snap_id -1
    }
    def get(index: Int, id: Int): Int =
      if(id == -1) 0
      else if(table.contains((id, index))) table((id,index))
      else get(index,id-1)

  }
}
