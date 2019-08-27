/**
  * Created by lilisun on 3/5/19.
  */
import scala.collection.mutable
import java.io.PrintWriter
import java.io.File

object makeReadME {

  val Source = scala.io.Source
  def subDir(dir: File): Iterator[File] = {
    val dirs = dir.listFiles().filter(_.isDirectory)
    val files = dir.listFiles().filter(_.isFile)
    files.toIterator ++ dirs.toIterator.flatMap(subDir)
  }
  def filesOfDir(dir: File): Iterator[File] = {
    dir.listFiles().filter(_.isFile).toIterator
  }
  def filesOfDir(dir: String): Iterator[File] = filesOfDir(new File(dir))
  def testFilesOfDir():Unit = {
    filesOfDir(new File("report/")) foreach println
  }
  def testSubDir():Unit = {
    subDir(new File(".")) foreach println
  }
  def buildMap(files: String, namePattern: String):mutable.HashMap[Int, String] = buildMap(filesOfDir(files), namePattern)
  def buildMap(files: Iterator[File], namePattern: String):mutable.HashMap[Int, String] = {
    val ans = mutable.HashMap[Int, String]()
    for{
      f <- files
      if f.getName matches namePattern
    }
      ans.put( f.getName.replaceAll("[a-zA-Z.]*", "").toInt, f.getPath)
    ans
  }
  // # Deprecated
  def parseRawData(content: String): List[(Int, String)] = {
    var words = content.split(" |\t")
    words = words.map(x => if(x matches "[0-9.]*%$") "" else x).filter(_ != "")
    var id = List[Int]()
    var name = List[String]()
    var tname = ""
    var preIsID = false
    for(word <- words){
      if(word matches "[0-9]*"){
        id = word.toInt :: id
        preIsID = true
      }else{
        if(preIsID){
          if(tname != "") name = tname::name //skip first
          tname = word
          preIsID = false
        }else{
          tname = tname + " " + word
        }
      }
    }
    // add last
    name = tname ::name

   id zip name
  }

  // This parser works for index file which has a format like:
  // id0
  // name xx xx xx xx
  // id1
  // name xx xx xx xx
  // ...
  def parseData(lines:Iterator[String], map:mutable.HashMap[Int, String]):mutable.HashMap[Int, String] =
  if(!lines.hasNext) map else  {
    val id = lines.next().stripLineEnd.toInt
    val tmp = lines.next().split("\t")
    //println(tmp.length, tmp.toList)
    val name = tmp.head//lines.next().split("\t").filter(_=="").head
    //println(id, name)
    map.put(id, name.trim)
    parseData(lines, map)
  }
  def parseData(lines:Iterator[String]):mutable.HashMap[Int, String] = {
    val map = mutable.HashMap[Int, String]()
    parseData(lines, map)
  }

  def buildIDPMap(filesDir: String):mutable.HashMap[Int, String] = buildIDPMap(filesOfDir(filesDir))
  def buildIDPMap(files: Iterator[File]):mutable.HashMap[Int, String] = {
    val file = files.filter(_.getName ==  "leetcode_problems_all.txt").next() // Modify here to mkChanges for index file
    val itr = Source.fromFile(file)
    /*
    println(file.getName)
    var i = 0
    for {x <- itr.getLines()}{
      println(i, x)
      i+= 1
    }*/
    parseData(itr.getLines())
  }
  def generateReadMe():Unit = {
    val pidMap = buildIDPMap("rawdata/")
    val solMap = buildMap("sol-module/src", "No[0-9]*.scala")
    val repMap = buildMap("report/", "ReportNo[0-9]*.md")
    println(s"Total Problem: ${pidMap.size}, Solved: ${solMap.size}, Report Num: ${repMap.size}")
    val text = List( "#  Leetcode Solutions with Scala ",
      "-------------------------------",
      "| # | Title | code | report| ",
      "|---| :-----: | :--------: | :----------: |")

    val writer = new PrintWriter(new File("README.md"))
    for(t <- text) writer.println(t)
    for(id <- solMap.keySet.toList.sortBy(x => -x)){
      val name = pidMap.getOrElse(id, "")
      val report = repMap.getOrElse(id, "TO DO")

      writer.println(s"|$id|$name|[Scala](${solMap(id)})|[Report]($report)|")
    }

    writer.close()

    println(s"Successfully update ReadMe file!")
    val path = "/Users/lilisun/Documents/tmp_leetcode_unsolved.txt"
    val unsolved_file = new PrintWriter(new File(path))
    unsolved_file.println(s"Unsolved problem:")
    pidMap.keySet.toList.filterNot {solMap.keySet.contains}.sorted.reverse foreach unsolved_file.println
    unsolved_file.close()

    println(s"Written unsolved id to file: $path")
  }
  def main(args: Array[String]): Unit = {
    generateReadMe()
  }
}
