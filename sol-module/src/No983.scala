/**
  * Created by lilisun on 3/4/19.
  */
object No983 {
  def mincostTickets(days: Array[Int], costs: Array[Int]): Int = {
    val dp = Array.ofDim[Int](366)
    val isVacation = Array.ofDim[Boolean](366)
    for(day <- days) isVacation(day) = true
    for(i <- 1 to 365){
      if(isVacation(i))
      dp(i) = dp(i-1) + costs(0) min costs(1)+ (if(i-7 >=0) dp(i-7) else 0) min costs(2)+(if(i-30 >= 0) dp(i-30) else 0 )
      else
        dp(i) = dp(i-1)
    }
    dp(365)
  }

  def main(args: Array[String]): Unit ={
    val days = Array(1,2,3,4,5,6,7,8,9,10,30,31)
    val costs= Array(2,7,15)
    println(mincostTickets(days, costs))
  }
}
