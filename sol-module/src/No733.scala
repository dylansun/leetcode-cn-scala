/**
  * Created by lilisun on 3/10/19.
  */
object No733 {
  def floodFill(image: Array[Array[Int]], sr: Int, sc: Int, newColor: Int): Array[Array[Int]] = {
    val oldColor = image(sr)(sc)
    image(sr)(sc) = newColor
    if(oldColor == newColor) return image // 如果不加会死循环
    var task = List((sr,sc))
    val step = Array((0,1),(0,-1), (1,0), (-1,0))
    while(task.nonEmpty){
      val todo = task.head
      task = task.tail
      step.foreach(dir => {
        val np = (todo._1 + dir._1, todo._2 + dir._2)
        if(image.indices.contains(np._1) && image(np._1).indices.contains(np._2) && image(np._1)(np._2) == oldColor){
          image(np._1)(np._2) = newColor
          task ::= np
        }
      })
    }
    image
  }
}
