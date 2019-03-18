/**
  * Created by lilisun on 3/19/19.
  */
object No951 {
  def flipEquiv(root1: TreeNode, root2: TreeNode): Boolean = (getValue(root1), getValue(root2)) match {
    case (None, None) => true
    case (Some(x), Some(y)) => x == y match {
      case false => false
      case true => {
        getValue(root1.left) == getValue(root2.left) match {
          case true => flipEquiv(root1.left, root2.left) && flipEquiv(root1.right, root2.right)
          case false => flipEquiv(root1.left, root2.right) && flipEquiv(root1.right, root2.left)
        }
      }
    }
    case _ => false
  }
  def getValue(root: TreeNode):Option[Int] = root match {
    case null => None
    case _ => Some(root.value)
  }

  def main(args: Array[String]): Unit = {
    val a = Some(1)
    val b = Some(1)
    val c = Some(2)
    val d:Option[Int] = None

    def f(a: Option[Int], b: Option[Int]): Boolean = {
      (a, b) match {
        case (None, None) => true
        case (Some(x), Some(y)) => x == y
        case _ => false
      }
    }

    println(f(a, b), f(a, c), a == d)
  }

}
