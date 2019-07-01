/**
  * Created by lilisun on 5/13/19.
  */
object No468 {
  object Solution {
    object IPV4{
      def unapply(s:String):Option[(List[Int])] = {
        val part = s split ('.')
        if(part.length != 4 || (part exists {x =>  (x.length <= 0 || x.length > 3)||(x exists {ch => !ch.isDigit})})) None
        else Some(part.toList.map(_.toInt))
      }
    }
    object IPV6{
      def unapply(s:String):Option[(List[String])] = {
        if(s.count(_==':') != 7) return None
        val part = s split (':')
        println(part.zipWithIndex.toList)
        if(part.length != 8 || (part exists {x => x.length > 4 ||(x exists {ch => !isHex(ch)})})) None
        else Some(part.toList)
      }
    }
    def isHex(ch:Char):Boolean = { ch.isDigit || (ch.toLower >= 'a' && ch.toLower <= 'f')}
    def validIPAddress(IP: String): String = IP match {
      case IPV4(l) if l forall {x => x <= 255}=> "IPv4"
      case IPV6(l) => "IPv6"
      case _ => "Neither"
    }
  }

  def main(args: Array[String]): Unit = {
    Solution.validIPAddress("2001:0db8:85a3:0:0:8A2E:0370:7334:")
  }
}
