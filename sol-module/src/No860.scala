/**
  * Created by lilisun on 1/11/19.
  */
object No860 {
  def lemonadeChange(bills: Array[Int]): Boolean = {
    var five = 0
    var ten = 0
    for(bill <- bills) bill match {
      case 5 => five = five + 1
      case 10 =>
        if(five > 0) {
          five = five - 1
          ten = ten + 1
        } else return false
      case 20 =>
        if(five > 0 && ten > 0) {
          five = five -1
          ten = ten -1
        }else if(five >= 3 && ten == 0){
          five = five - 3
        }else return false
    }
    true
  }
}
