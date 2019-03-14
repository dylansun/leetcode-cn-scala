/**
  * Created by lilisun on 3/14/19.
  */
object No957 {
  def prisonAfterNDays(cells: Array[Int], N: Int): Array[Int] = {
    val n = if(N % 14 == 0 && N > 14) 14 else N % 14
    n  match {
      case 0 => cells
      case _ => {
        val newCell = Array.fill(cells.length)(0)
        for(i <- 1 until cells.length -1) newCell(i) = if(cells(i-1) == cells(i+1)) 1 else 0
        println(s"${newCell.toList}, $n")
        prisonAfterNDays(newCell, n -1)
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val cells = Array(0,1,0,1,1,0,0,1)
    println(prisonAfterNDays(cells, 7).toList)
  }
}
