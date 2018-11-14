import scala.collection.mutable.HashSet

object No30 {

  /**
    * TODO: optimize speed
    * @param s
    * @param words
    * @return
    */
  def findSubstring(s: String, words: Array[String]): List[Int] = {
    if(s.isEmpty || words.isEmpty) return Nil
    val l1 = s.length
    val l2 = words.map(_.length).sum
    val targets = words.permutations.map(_.mkString).toSet

    val hash_targets = new HashSet[String]()
    val it = targets.iterator
    while(it.hasNext){
      hash_targets.add(it.next())
    }

    (for (i <- 0 to (l1 - l2) if targets.contains(s.substring(i, i+l2))) yield i).toList


  }



  def main(args: Array[String]): Unit = {
    val s = "1234432534635465347347134444123"
    val words = Array("1","4","4")
    val l1 = s.length
    val l2 = words.map(_.length).sum
    val targets = words.permutations.map(_.mkString).toList
    println(targets.mkString)

    println("ssss")
    val hash_targets = new HashSet[String]()
    val it = targets.iterator
    while(it.hasNext){

      hash_targets.add(it.next())

    }

    println(hash_targets.mkString)


    //val k = findSubstring(s,words)
    //println(k)

  }
}
