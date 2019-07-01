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
  object Solution {
    // s0, s1,.. si, sj, sj+1, ...sj+k,
    def prisonAfterNDays(A: Array[Int], N: Int): Array[Int] = {
      var l = List(A)
      def next(A:Array[Int]):Array[Int] = {
        val ans = Array.fill(8)(0)
        for {i <- 1 to 6} ans(i) = (A(i-1), A(i+1)) match {
          case (0,0) => 1
          case (1,1) => 1
          case _ => 0
        }
        ans
      }

      def solve(l:List[Array[Int]], A:Array[Int]):Array[Int] = {
        val mem = l.reverse.toArray
        val n = l.length
        for{
          i <- 0 until n
          if mem(i).toList == A.toList
        }{
          // 0 1 2 3 4 5 6
          //       ^
          val rep = n - i
          val idx = ((N- i) %(n-i)) + i
          println(i, n , N)
          mem foreach {x => println(x.toList)}
          println("-----")
          println(A.toList)
          return mem(idx)
        }
        Array.empty[Int]
      }

      for (i <- 1 to N){
        val tmp = next(l.head)
        if(l exists (x => x.toList == tmp.toList)){
          return solve(l, tmp)
        }
        else l ::= next(l.head)
      }
      l.head

    }
  }

  def main(args: Array[String]): Unit = {
    val cells = Array(0,1,0,1,1,0,0,1)
    println(prisonAfterNDays(cells, 7).toList)
  }
}
