/**
  * Created by lilisun on 3/15/19.
  */
object No451 {
  def frequencySort(s: String): String = {
     if(s == "") return ""
      s
        .groupBy(x=>x)
        .values
        .toArray
        .sortBy(- _.length)
        .reduce(_+_)
  }

  def main(args: Array[String]): Unit = {
    println(frequencySort("aAbb"))
    println("a"*5)
  }
}
