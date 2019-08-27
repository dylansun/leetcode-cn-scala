/**
  * Defanging an IP Address
  */
object No1108 {
  def defangIPaddr(a: String): String = {
    a.replaceAll("\\.","[.]")
  }
}
