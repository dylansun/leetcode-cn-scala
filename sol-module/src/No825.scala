/**
  * Created by lilisun on 3/26/19.
  */
object No825 {
  case class Age(age:Int, n:Int)
  def numFriendRequests(ages: Array[Int]): Int = {
    solver(f(ages.filter(_ >= 15)).sortBy(x => -x.age), 0)
  }
  def solver(ages: Array[Age], acc:Int):Int = {
    if(ages.length == 0) acc else{
      ages.tail.indexWhere(x => x.age.toDouble <= 0.5 * ages.head.age + 7.0) match {
        case -1 => solverHelp(ages.tail, acc + send(ages.head.n) + ages.tail.map(x => x.n).foldLeft(0)(_+_) * ages.head.n)
        case bound:Int => solver(ages.tail, acc + send(ages.head.n) + ages.tail.slice(0,bound).map(x => x.n).foldLeft(0)(_+_) * ages.head.n)
      }
    }
  }

  def solverHelp(ages:Array[Age], acc:Int):Int = {
    if(ages.length == 0) acc else{
      solverHelp(ages.tail, acc + send(ages.head.n) + ages.tail.map(x => x.n).foldLeft(0)(_+_) * ages.head.n)
    }}
  def send(n:Int):Int = n match {
    case 1 => 0
    case _ => n*(n-1)
  }

  def f(ages: Array[Int]):Array[Age] ={
    val ac = scala.collection.mutable.HashMap[Int, Int]()
    ages.foreach(x => ac.put(x, ac.getOrElse(x, 0) + 1))
    (ac.keySet zip ac.values).map(x => Age(x._1, x._2)).toArray
  }
}
