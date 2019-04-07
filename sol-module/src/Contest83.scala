/**
  * Created by lilisun on 4/5/19.
  */
object Contest83 {
  //例 3:

//  输入: "bbbbb"
//  输出: [[3,5],[6,9],[12,14]]
  def largeGroupPositions(s: String): List[List[Int]] = {
    var pre = s(0)
    var pidx = 0
    var ans = List[List[Int]]()
    for(i <- 1 until s.length if s(i)!= pre){
      if(i - pidx >= 3) ans ::= List(pidx, i-1)
      pidx = i
      pre = s(i)
    }
    if(s.length - pidx >= 3)  ans ::= List(pidx, s.length - 1)
    ans.reverse
  }
  def maskPII(S: String): String = {
    if(S.contains('@')) fname(S) else fnum(S)
  }
//  "name1@name2.name3"
  def fname(s:String):String = {
    val ch = s.split('@')
    (ch(0).head + "*****" + ch(0).last +"@" + ch(1)).toLowerCase
  }
  def fnum(s:String):String = {
    fnumhelp(s.filter(x => ('0' to '9').toList.contains(x)))
  }
  // 012345
  def last4(s:String):String = {
    s.slice(s.length - 4, s.length)
  }
  def fnumhelp(s:String):String = {
    if(s.length == 10){
      "***-***-" + last4(s)
    }else{
      "+"+("*"*(s.length - 10))+"-***-***-" + last4(s)
    }
  }

  // a0 ... an-1
  // (a0 + an-1)* n / 2 = N
  // (a0 + a0 + n-1) * n /2 = N
  // 2*N / n = (a0 + an-1) // 1, n, >= n+ 1
  // x satisfy  2*N mod x == 0 && 2 * N / x >= x
  // a0 + an-1 = 2N / n
  // an-1 - a0 = n - 1
  // an-1 = (2N/n + n - 1 )  / 2
  // a0 = an-1 - n + 1

  // 9 *2 = 18
  // a0 + an-1 = 9
  // an- a0 = n-1
  // (i + 0.5) <= sqrt(2N +0.25)

  def consecutiveNumbersSum(N: Int): Int = {
    var ans = 0
    for{
      i <- 1 to (Math.sqrt(2 * N + 0.25)  - 0.5).toInt
      if (2 * N) % i == 0 && (2 * N) / i >= i +1  && ((2 * N) / i  + i -1) % 2 == 0// a0 + an-1
    } ans += 1
    ans
  }

  val mod = 1000000007
  // dp(i) 0123456789, end with i
  def uniqueLetterString(S: String): Int = {
    S.indices.map(f1(S.toArray)).foldLeft(0){(sum, x) => (sum + x) % mod}
  }

  def f1(A:Array[Char])(idx:Int):Int = {
    (0 to idx)
      .toArray
      .map(x => f2(A)(x,idx))
      .foldLeft(0){(sum, x) => (sum + x ) % mod}
  }

  def f2(A:Array[Char])(f:Int, t:Int):Int ={
    val table = Array.fill(52)(0)
    for(i <- f to t) table(trans(A(i))) += 1
    table.count(_==1)
  }

  def trans(ch:Char):Int = {
    if(ch >= 'a' && ch <= 'z') ch - 'a'
    else ch - 'A' + 26
  }


  def main(args: Array[String]): Unit = {
    ('A' to 'Z').foreach(x => println(x.toInt))
  }
}
