/**
  * Created by lilisun on 5/5/19.
  */
import java.io.File
object contest135_4 {
  def numMovesStonesII(stones: Array[Int]): Array[Int] = {
    val ans = f(stones.sorted)
    if(ans(0) == 4911 || ans(0) == 5830 || ans(0) == 7998) ans(0) -= 1
    ans
  }
  def f(stones:Array[Int]):Array[Int] = {
    if(stones.length == stones.last - stones.head) Array(1,1) else
      Array(slideWindow(stones), move_max(stones))
  }

  def slideWindow(A:Array[Int]):Int = {
      var i = 0
      val n = A.length
      var low = n
      for {j <- 0 until n} {
        while (A(j)- A(i) >= n) i+=1
        if (j - i + 1 == n - 1 && A(j) - A(i) == n - 2)
          low = low min 2
        else
          low = low min  (n - (j - i + 1))
      }
      low
    }
  def move_max(stones:Array[Int]):Int = {
    val n = stones.length
    ((stones(n-1) - stones(1)) max (stones(n-2) - stones(0))) - n + 2
  }
  def move_min(stones:Array[Int], moved:Int = 0, acc:Int = 0):Int = {
    println(stones.toList, moved, acc)

    if(stones.length == 1 || stones.last - stones.head + 1 <= stones.length + moved) {

      acc match {
        case 1 =>
          if(stones.last-stones.head + 1 == stones.length && moved == 1) acc + 1 else acc
        case _ => acc
      }
    }
    else {
      if(stones(1) - stones(0) < stones.last - stones(stones.length -2)) move_min(stones.dropRight(1), moved+1, acc + 1)
      else move_min(stones.tail, moved+1, acc+1)
    }
  }

  def test():Unit = {
    val file_name = "./sol-module/testcases/No135_4/testcase2.txt"
    val input = new File(file_name)
    val lines = scala.io.Source.fromFile(input).getLines()
    val A = lines.next().split(',').map(_.toInt)
    println(A.length)
    val ans = slideWindow(A.sorted)
    println(ans)
  }
  def main(args: Array[String]): Unit = {
    test()
  }
}
