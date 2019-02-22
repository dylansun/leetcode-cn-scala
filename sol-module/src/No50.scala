/**
  * Created by lilisun on 2/23/19.
  */
object No50 {
  def myPow(x: Double, n: Int): Double = {
    def myPowInAbs(x: Double, n: Int, acc: Double = 1.0): Double = {
      if (n == 0) 1.0 * acc
      else if (n == 1) x * acc
      else if (n % 2 == 0)
        myPowInAbs(x * x, n / 2, acc )
      else
        myPowInAbs(x * x, n / 2, acc * x)
    }

    if (n < 0)
      1 / myPowInAbs(x, n.abs)
    else
      myPowInAbs(x, n)
  }
}
