/**
  * Created by lilisun on 11/14/18.
  */
object No937 {

  def reorderLogFiles(logs: Array[String]): Array[String] = {
    logs
      .filter(_.replaceFirst("[a-z0-9]* ", "") matches  "[a-z ]*")
      .sortBy(_.replaceFirst("[a-z0-9]* ", "")) ++
    logs
      .filter(_.replaceFirst("[a-z0-9]* ", "") matches  "[0-9 ]*")
  }
}
