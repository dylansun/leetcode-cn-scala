/**
  * Created by lilisun on 3/23/19.
  */
object No878 {
  def nthMagicalNumber(n: Int, a: Int, b: Int): Int = {
    val l = lcm(a,b)
    val MOD = 1000000007
    val n_sub = lenOfsub(l, a, b)
    var p = n / n_sub
    var r = n % n_sub
    if(r == 0){r = n_sub -1;p = p -1}else r = r -1
    val rem = findR(a,b)(a,b,r)
    val ans  = (BigInt(p) * BigInt(lcm(a,b)) % MOD  + rem) % MOD
    ans.toInt
  }

  def findR(a:Int, b:Int)(x:Int, y:Int, pos:Int):Int = pos match {
    case 0 => x min y
    case _ => x > y match {
      case true => findR(a,b)(x, b+y, pos -1)
      case _    => findR(a,b)(a+x, y, pos -1)
    }
  }

  def lenOfsub(l:Int, a:Int, b:Int):Int = {
    l / a + l / b - 1
  }
  def lcm(a:Int, b:Int) = a * b / gcd(a,b)
  def gcd(a:Int, b:Int):Int = a match {
    case 0 => b
    case _ => gcd(b % a, a)
  }
  def test():Unit = {
    val x = 12 * 11 * 7
    val y = 7 * 13 * 19
    println(gcd(x,y), lcm(x,y))
  }

  def test2():Unit = {
    println(nthMagicalNumber(3,6,4))
    println(nthMagicalNumber(4,2,3))
    println(nthMagicalNumber(1,2,3))
    println(nthMagicalNumber(5,2,4))
  }

  def main(args: Array[String]): Unit = {
    test2()
  }
}
