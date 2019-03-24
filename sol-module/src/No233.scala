/**
  * Created by lilisun on 3/23/19.
  */
object No233 {
  def countDigitOne(num: Int): Int = {
    if(num < 1) return  0
    if(num < 10) return 1
    val n = num.toString.length
    val dp = Array.fill(n+1, 10)(0)
    for(i <- 1 to n ) for(j <- 0 to 9) j match {
      case 1 => dp(i)(j) = Math.pow(10, i-1).toInt + (for(k<- 0 to 9) yield dp(i-1)(k)).sum
      case _ => dp(i)(j) = (for(k <- 0 to 9) yield dp(i -1)(k)).sum
    }

    var numstr = num.toString.toList
    var ans = 0
    while(numstr.nonEmpty){
      val head = numstr.head - '0'
      if(numstr.tail.nonEmpty) for(i <- 0 until head) ans += dp(numstr.length)(i)
      (head, numstr.tail) match {
        case (_, Nil) => ans += (if(head >= 1) 1 else 0) // last digit
        case (1, _)  => ans += numstr.tail.mkString.toInt + 1
        case (_,_ ) => {}
      }
      numstr = numstr.tail
    }
    ans
  }

  def ans(n: Int): Int = {
    (1 to n).flatMap(_.toString.split("")).count(_=="1")
  }

  def main(args: Array[String]): Unit = {
    (1 to 21345 ).foreach(n => if(countDigitOne(n) != ans(n)) println(s"fail $n"))
  }
}
