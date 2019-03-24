/**
  * Created by lilisun on 3/23/19.
  */
object No902 {
  def atMostNGivenDigitSet(D: Array[String], N: Int): Int = {
    val n = N.toString.length
    var ans = 0
    for(i <- 1 until n) ans += Math.pow(D.length, i).toInt
   /* var num = N.toString.toList
    while(num.nonEmpty) {
      println(ans)
      ans +=  D.count(x => x.toInt < num.head - '0') * Math.pow( D.length,num.tail.length).toInt
      if(D.contains(num.head.toString)) {
        if(num.tail.isEmpty)
          ans += (if(D.contains(num.head.toString)) 1 else 0)
        num = num.tail
      }
      else num = Nil
    }
    ans*/
    countLenN(D.map(_.toInt), N.toString.map(_ - '0').toList, ans)
  }

  def countLenN(nums: Array[Int], l: List[Int], acc:Int ):Int  = l match {
    case Nil => acc
    case h::Nil => acc + nums.count(_ <= h)
    case h::t => {
      val ans = nums.count(_ < h)*Math.pow(nums.length, l.tail.length).toInt
      nums.contains(h) match {
        case false => acc + ans
        case true => countLenN(nums, l.tail, acc + ans)
      }
    }
  }








}
