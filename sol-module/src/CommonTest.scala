/**
  * Created by lilisun on 2/14/19.
  */
import collection.immutable.Stack
object CommonTest {

  def solve(meal_cost: Double, tip_percent: Int, tax_percent: Int) {

    val ans = meal_cost + meal_cost * tip_percent / 100 + meal_cost * tax_percent / 100
    //println( 10.75 * 1.22)
    println(Math.round(ans))

  }

  def main(args: Array[String]): Unit = {
    val s = new MinStack()
    s.push(-2)
    s.push(-3)
    s.push(0)
    s.push(-4)
    solve(10.75, 17, 5)

  }
}
