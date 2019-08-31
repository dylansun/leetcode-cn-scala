/**
  * 609. 在系统中查找重复文件
  */
object No609 {
    def findDuplicate(paths: Array[String]): List[List[String]] = {
      val table = scala.collection.mutable.HashMap[String, List[String]]()

      def g(path:String)(l:List[String]):Unit = l match {
        case Nil => {}
        case h::t =>
          val content = h.dropWhile(_ != '(').tail.dropRight(1)
          val fullpath = path + "/" + h.takeWhile(_ != '(')
          table.put(content, fullpath::table.getOrElse(content, Nil))
          g(path)(t)
      }
      paths foreach { str => str split " " toList match {
        case h::t => g(h)(t)
      }}

      (for{ (k, v) <- table if v.length > 1} yield v).toList
    }

  def main(args: Array[String]): Unit = {
    println("So cool!")
  }
}
