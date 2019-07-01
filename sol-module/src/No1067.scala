/**
  * Created by lilisun on 6/2/19.
  */
object No1067 {
  //
  val dp = calAll(9)
  // return n bit digit, including leading 0
  // 0 vs. 1~9
  // ex. 4 digits
  //      0 0 0 0
  //      0 0 0 1
  //      .......
  //      9 9 9 9
  //  add one digit:
  //      ? x x x
  //      1 * 10 ^ 3 + 10 * f(3)
  def calAll(n:Int):Array[Int] = {
    val dp = Array.fill(n+1)(0) // digit occurrence of length i
    dp(0) = 1 // set the 0th value to 1, since we will count the number of one-bit digit as dp(0) times
    dp(1) = 1
    for{i <- 2 to n} dp(i) = 10 * dp(i-1) + Math.pow(10, i-1).toInt // this is d vs. that
    dp
  }
  def digitsCount(d: Int, low: Int, high: Int): Int = {
    f(high)(d) - f(low - 1)(d)
  }
  def f(x:Int):Array[Int] = {
    val cnt = Array.fill(10)(0)
    if(x < 0) return cnt
    val digits = x.toString.toCharArray.map(_ - '0')
    val m = digits.length
    // 3 2 1, for number 321
    // part 1:
    // 0 x x,
    // 1 x x,
    // 2 x x
    // part 2:
    // 3 0 x
    // 3 1 x
    // 3 2 0
    // 3 2 1
    //cnt.indices foreach {i => cnt(i) += dp(m-1)}
    for{
      i <- digits.indices
      d = digits(i)
      n_bit = m - i - 1
    }
    {
      println("-----------")
      println(d, n_bit , i)
      println(cnt.toList)
      (0 to 9).foreach(x => if(n_bit > 0) cnt(x) += d * dp(n_bit))
      (0 until d).foreach(x => cnt(x) += Math.pow(10, n_bit).toInt)
      //(0 until d) foreach {digit => cnt(digit) += (d - 1) * dp(n_bit) + Math.pow(10, n_bit).toInt} //
      println(cnt.toList)
      cnt(d) += 1 + (if(n_bit > 0) digits.slice(m - n_bit,m).foldLeft(0){(acc, x) => acc * 10 + x} else 0)// m - ? = i
      println(cnt.toList)

    }
    println("------------")
    // remove leading zero
    // for a m-length digit
    // 0 x x x 1 * 10 ^ (m - 1)
    // 0 0 x x 1 * 10 ^ (m - 2)
    // 0 0 0 x 1 * 10 ^ (m - 3)
    for{i <- 1 until m} cnt(0) -= Math.pow(10, i).toInt
    println(cnt.toList)
    cnt
  }

  def ans(d:Int, l:Int, r:Int):Seq[Int] = {
    for{d <- 0 to 9 } yield (l to r ).map(x => x.toString.count(ch => ch - '0' == d)).sum
  }

  def main(args: Array[String]): Unit = {
   // println(ans(1,0,1234567))
    println(digitsCount(1,0,1234567))
   // println(ans(3,100, 250))
   // println(digitsCount(3, 100, 250))
    println(dp.toList)
    println(ans(1,10,999))
  }
}
