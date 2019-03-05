/**
  * Created by lilisun on 3/5/19.
  */
import scala.collection.mutable
import java.io.PrintWriter
import java.io.File

object makeReadME {

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
    val dir = new File("report/")
    filesOfDir(dir).foreach(println)
  }
  def testSubDir():Unit = {
    val dir:File = new File(".")
    for( d <- subDir(dir)) println(d)
  }
  def buildMap(files: String, namePattern: String):mutable.HashMap[Int, String] = buildMap(filesOfDir(files), namePattern)
  def buildMap(files: Iterator[File], namePattern: String):mutable.HashMap[Int, String] = {
    val ans = mutable.HashMap[Int, String]()
    for(f <- files){
      if(f.getName matches namePattern){
        val id = f.getName.replaceAll("[a-zA-Z.]*", "").toInt
        ans.put(id, f.getPath)
      }
    }
    ans
  }
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
  def buildIDPMap(filesDir: String):mutable.HashMap[Int, String] = buildIDPMap(filesOfDir(filesDir))
  def buildIDPMap(files: Iterator[File]):mutable.HashMap[Int, String] = {
    val pidMap = mutable.HashMap[Int, String]()
    for( file <- files){
      val source = scala.io.Source.fromFile(file.getPath)
      val lines = source.getLines()
      var content = ""
      for(line <- lines) content = content + " " + line
      val idp = parseRawData(content)
      for(kv <- idp) pidMap(kv._1) =kv._2
    }
    pidMap
  }
  def generateReadMe():Unit = {
    val pidMap = buildIDPMap("rawdata/")
    val solMap = buildMap("sol-module/src", "No[0-9]*.scala")
    val repMap = buildMap("report/", "ReportNo[0-9]*.md")
    println(pidMap.size, solMap.size, repMap.size)
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
  }
  def main(args: Array[String]): Unit = {
    generateReadMe()
  }
}
