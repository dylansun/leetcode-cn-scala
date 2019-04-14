/**
  * Created by lilisun on 4/12/19.
  */
object No900 {
  class RLEIterator(_A: Array[Int]) {
    val A = _A
    var iter = 0
    def next(n: Int): Int = {
      if(iter > A.length -1 || n <= 0) -1
      else{
        if(n <= A(iter)){
          A(iter) -= n
          A(iter + 1)
        }else{
          iter += 2

          next(n - A(iter - 2))
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val iter = new RLEIterator(Array(3,8,0,9,2,5))
    for(x <- List(2,1,1,2)){
      println(iter.next(x))
      println(iter.A.toList, iter.iter, x)
    }
  }
}
