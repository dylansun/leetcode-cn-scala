/**
  * Created by lilisun on 2/19/19.
  */
object No135 {
  def candy(ratings: Array[Int]): Int = {
    val cands = ratings.map( x => 1)

    for(x <- 1 until ratings.length
        if ratings(x) > ratings(x-1) && cands(x) <= cands(x-1))
      cands(x) = cands(x-1) + 1

    for(x <- (0 until ratings.length -1).reverse
        if ratings(x) > ratings(x+1) && cands(x) <= ratings(x+1))
      cands(x) = cands(x+1) +1

    cands.sum
  }

  def main(args: Array[String]): Unit = {
    val cands = Array(1,2,87,87,87,2,1)
    println(candy(cands))
  }
}
