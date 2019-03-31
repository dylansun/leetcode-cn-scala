import com.sun.prism.impl.Disposer.Target

/**
  * Created by lilisun on 3/29/19.
  */
object No18 {
  def fourSum(nums: Array[Int], target: Int): List[List[Int]] = {
    solve(nums.sorted, target)
  }
  def solve(nums: Array[Int], target: Int): List[List[Int]] = {
    nums.indices.flatMap( i =>f(i+1, nums, target - nums(i)) match {
      case Nil => Nil
      case l => for(li <- l) yield nums(i)::li
    }).filter(_.nonEmpty).toList.distinct
  }

  def f(i:Int,nums:Array[Int], target: Int):List[List[Int]] = {
    (i until nums.length).flatMap( i => g(i+1, nums, target - nums(i)) match {
      //case Nil => Nil
      case l => for(li <- l) yield nums(i)::li
    }).filter(_.nonEmpty).toList
  }
  // find  (x,y) in i to n-1 where nums(x)+nums(y) = target
  def g(i:Int, nums:Array[Int], target:Int):List[List[Int]] = {
    //val table = scala.collection.mutable.HashMap
    (for {x <- i until nums.length
          y <- x + 1 until nums.length
          if nums(x) + nums(y) == target
    }yield List(nums(x),nums(y))
      ).filter(_.nonEmpty).toList
  }

  def k(nums:Array[Int], target: Int)={//:List[List[Int]] = {
    (for{ i <- nums.indices
         j <- i+1 until nums.length
         k <- j+1 until nums.length
         l <- k+1 until nums.length
        if nums(i) + nums(j)+nums(k) + nums(l) == target
    } yield List(nums(i), nums(j), nums(k), nums(l))).toSet.toList
  }


  def main(args: Array[String]): Unit = {
    val nums = Array(-5,5,4,-3,0,0,4,-2).sorted
    val targe = 4
    fourSum(nums.sorted, targe).filter(_.nonEmpty).foreach(println)

    println( List(1) == List(1))

    val l = List(Nil, List(1))
    println(l.flatten)

    def ff(x:Int) = x match {
      case 1 => List[List[Int]]()
      case _ => List((0 to x).toList)
    }
    println((1 to 3).toList.flatMap{ff})
  }
}
