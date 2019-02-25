/**
  * Created by lilisun on 2/25/19.
  */
object No198 {
  /**
  case class Memo[I <% K, K, O](f: I => O) extends (I => O) {
    import collection.mutable.{Map => Dict}
    val cache = Dict.empty[K, O]
    override def apply(x: I) = cache getOrElseUpdate (x, f(x))
  }

  def subsetSum(s: List[Int], t: Int) = {
    type DP = Memo[(List[Int], Int), (Int, Int), Seq[Seq[Int]]]
    implicit def encode(key: (List[Int], Int)) = (key._1.length, key._2)

    lazy val f: DP = Memo {
      case (Nil, 0) => Seq(Nil)
      case (Nil, _) => Nil
      case (a :: as, x) => (f(as, x - a) map {_ :+ a}) ++ f(as, x)
    }

    f(s, t)
  }*/

  def rob(nums: Array[Int]): Int =  nums.length match {
    case 0 => 0
    case 1 => nums(0)
    case 2 => nums.max
    case _ => {
      var last = nums(0)
      var now = nums(1)
      for( x <- 2 until nums.length){
        val cur = last + nums(x)
        last = now max last
        now = cur
      }
      last max now
    }

  }

  def main(args: Array[String]): Unit = {
    val t = Array(2,7, 9,3,1)
    println(rob(t))
    val m = scala.collection.mutable.Map()

  }
}
