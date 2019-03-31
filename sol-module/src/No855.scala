/**
  * Created by lilisun on 3/26/19.
  */
object No855 {
  class ExamRoom_Array(_N: Int) {
    val seats = Array.fill(_N)(0)
    def seat(): Int = {
      val next = find
      seats(next) = 1
      next
    }
    def find:Int = {
      if(findOne.length == 0) 0
      else findZero
        .map(x => (findOne.map( y => Math.abs(y._2 - x._2)).min, x._2))
        .foldLeft((0, 0))( (t, x) => if(x._1 > t._1) x else t)._2
    }

    def findOne:Array[(Int, Int)] = seats.zipWithIndex.filter(x => x._1 == 1)
    def findZero: Array[(Int,Int)] = seats.zipWithIndex.filter(x => x._1 == 0)
    def leave(p: Int) {
      seats(p) = 0
    }
  }
  class ExamRoom_List(N:Int){
    var band:List[Int] = (0 until N).toList
    var person = List[Int]()
    def seat():Int = person match {
      case Nil =>
        person ::= 0
        band = band.filterNot(_==0)
        0
      case h::Nil =>
        val choose = if(h > (N-1) -h) 0 else N-1
        person ::= choose
        person = person.filter(_ < choose) ++ List(choose) ++ person.filter(_ > choose)
        band = band.filterNot(_==choose)
        choose
      case _ =>
        val can = (person zip person.tail).foldLeft((-1,-1)){
          (ans, x) => if( (x._2 - x._1) / 2 > ans._1)  ((x._2 - x._1) / 2 , (x._1 + x._2) /2) else ans
        }
        //.map(x => ((x._2 - x._1) / 2 , (x._1 + x._2) /2)).sortBy(x => (-x._1, x._2)).head
        var cans = can::Nil
        if(person.head != 0) cans ::= (person.head - 0, 0 )
        if(person.last != N-1) cans ::= (N - 1 - person.last, N-1)
        val choose = cans.sortBy(x => (-x._1, x._2)).head._2
        band = band.filterNot(_==choose)
        person ::= choose
        person = person.filter(_ < choose) ++ List(choose) ++ person.filter(_ > choose)
        choose
    }
    def leave(p:Int): Unit ={
      person = person.filterNot(_==p)
      band ::= p
    }
  }

  // todo using priority queque, the task will be done in O(logn) for seat and o(N) for leave
  class ExamRoom(N:Int){
    scala.collection.mutable.PriorityQueue[Int]()

  }
  def main(args: Array[String]): Unit = {
    val room = new ExamRoom_List(10)
    for(_ <- 1 to 10) println(room.seat())
  }
}
