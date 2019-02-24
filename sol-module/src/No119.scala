/**
  * Created by lilisun on 2/25/19.
  */
object No119 {
  def getRow(rowIndex: Int): List[Int] = solver(rowIndex, List[Int](1))

  //(for( x<- 0 to rowIndex) yield binomial(rowIndex, x)).toList

  def solver(i: Int, acc: List[Int]): List[Int] = i match {
    case 0 => acc
    case _ =>  //solver(i-1, 1 :: (for(x <- 0 to acc.length - 2) yield acc(x)+ acc(x+1)).toList ::: List(1))
        solver(i-1, 1:: (0 to acc.length -2).foldLeft(List[Int]())((l,x)=> acc(x)+ acc(x+1) :: l) :::List(1))
  }

  def binomial(n: Int, k:Int):Int = factorial(n)/factorial(k) / factorial(n-k)
  def factorial(n: Int, acc:Int = 1):Int = n match {
    case 1 | 0=> acc
    case _ => factorial(n-1, acc * n)
  }
  def main(args: Array[String]): Unit = {
    (0 to 10).foreach(x => println(getRow(x)))
    (0 to 3).foreach(x => print(s"${binomial(3,x)} "))
  }
}
