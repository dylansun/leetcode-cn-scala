/**
  * Created by lilisun on 8/29/19.
  */
object No388 {
  object Solution {
    case class Elem(level:Int, name:String){
      def isFile():Boolean = name.contains('.')
      def getLength():Int = if(this.isFile) name.length else name.length + 1
    }

    def trans(str:String):Elem = {
      val name = str.replaceAll("\t", "")
      Elem((str.length - name.length), name)
    }

    def lengthLongestPath(input: String): Int = {
      var ans = 0
      val len = scala.collection.mutable.HashMap[Int, Int]()
      def f(elem:Elem):Unit = {
        len.put(elem.level, elem.getLength())
        if(elem.isFile()) ans = ans max ((0 to elem.level) map len sum)
      }
      input.split("\n") map trans foreach f
      ans
    }
  }
}
