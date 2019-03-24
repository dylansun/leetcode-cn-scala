/**
  * Created by lilisun on 3/24/19.
  */
object No1021 {

  def smallestRepunitDivByK(K: Int): Int = {
    if(K == 1) return 1

    K % 10 match {
      case 1 => solver(K/10,K, K/10 % 10,1)
      case 3 => solver(K*7/10,K, K*7/10 % 10,1)
      case 7 => solver(K*3/10,K, K*3/10 % 10,1)
      case 9 => solver(K*9/10,K, K*9/10 % 10,1)
      case _ => -1
    }
  }

  def solver(cur:Int, base: Int, last:Int, acc:Int): Int = {
    if(cur == 1 && last == 1) return acc + 1
    if(cur == 0 && last == 0) return acc
    val bits = (0 to 9).toList
    val candidate = bits.find(x => (x * base + last) % 10 == 1).getOrElse(-1)
    if(candidate < 0)  -1
    else{
      val res = (candidate * base + cur) / 10
      solver( res , base ,res % 10 , acc + 1)

    }

  }
  def main(args: Array[String]): Unit = {
    println(smallestRepunitDivByK(49993))
  }
}
