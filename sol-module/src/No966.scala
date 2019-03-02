/**
  * Created by lilisun on 3/2/19.
  */

import scala.collection.mutable
object No966 {

  def spellchecker(wordlist: Array[String], queries: Array[String]): Array[String] = {
    val l0Map = mutable.HashMap[String, String]()
    val l1Map = mutable.HashMap[String, String]()
    val l2Map = mutable.HashMap[String, String]()
    //
    for(word <- wordlist) l0Map.put(word, word)
    for(word <- wordlist) if(!l1Map.contains(word.toLowerCase()))l1Map.put(word.toLowerCase(), word)
    for(word <- wordlist){
      val  x = vowelExpansion(word.toLowerCase())
      if(!l2Map.contains(x)) l2Map.put(x, word)
    }

    println()
    l2Map.foreach(println)
    queries.map( query => {
      if(l0Map.contains(query)) l0Map(query)
      else if(l1Map.contains(query.toLowerCase())) l1Map(query.toLowerCase())
      else if(l2Map.contains(vowelExpansion(query.toLowerCase()))) l2Map(vowelExpansion(query.toLowerCase()))
      else ""
    }
    )
  }
  def vowelExpansion(word: String): String = {
    val vowel = List('a','e','i','o','u')
    (for( ch <- word) yield {
      if (vowel.contains(ch)) '0' else ch
    }).toString
  }

  def spellchecker_TLE(wordlist: Array[String], queries: Array[String]): Array[String] = {
    queries.map( query => {
      val t = wordlist.map(word => stringCompare(word,query)).zipWithIndex.sortBy(_._1).head
      if(t._1 == 4) -1 else t._2
    }).map(x => if(x >= 0) wordlist(x) else "")
  }

  // return priority
  def stringCompare(word: String, query: String): Int = {
    if(word.length != query.length) return 4
    if(word == query) return 0
    if(word.toLowerCase() == query.toLowerCase()) return 1
    if(isVowelErrors(word.toLowerCase(), query.toLowerCase())) return 3
    4
  }

  def isVowelErrors(word: String, query: String):Boolean = {
    val vowel = List('a','e','i','u','o') // WA, 少写了一个o TAT
    for(x <- word.indices if word(x) != query(x) && !(vowel.contains(word(x)) && vowel.contains(query(x)) )){
      return false
    }
      true
  }

  def main(args: Array[String]): Unit = {
    val wordList = Array("KiTe","kite","hare","Hare")
    val queries = Array("kite","Kite","KiTe","Hare","HARE","Hear","hear","keti","keet","keto")

    val query = queries(0)
   // val r1 = wordList.map(word => stringCompare(word, query)).zipWithIndex.sortBy(_._1)
    //println(r1.mkString)
   val res = spellchecker(wordList, queries)
    val t = queries.map(x => vowelExpansion(x.toLowerCase()))
    t.foreach(println)
    println()
    res.foreach(println)

  }
}
