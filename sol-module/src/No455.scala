/**
  * Created by lilisun on 2/22/19.
  */
object No455 {
  def findContentChildren(g: Array[Int], s: Array[Int]): Int = {
    val gs = g.sorted
    val ss = s.sorted
    var i=0; var j =0
    while(i < gs.length && j < ss.length){
      if(gs(i) <= ss(j)) i += 1
      j+=1
    }
    i
  }
}
