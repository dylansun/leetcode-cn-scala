/**
  * Created by lilisun on 1/10/19.
  */
object No49 {
  def groupAnagrams(strs: Array[String]): List[List[String]] = {
    (for(c <- strs.map(x => (x.sorted, x)).groupBy(_._1))
      yield c._2.map(_._2).toList).toList
  }

  def main(args: Array[String]): Unit = {
    val s = Array[String]("eat","tea","tan","ate","nat","bat")
    //val sk = s.map(x => (x.sorted, x)).groupBy(_._1)
    //val skl = for(c <- sk) yield c._2.map(_._2).toList
    println(groupAnagrams(s))
  }
}
