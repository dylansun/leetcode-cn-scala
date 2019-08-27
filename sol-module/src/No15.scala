object No15 {
  // TLE: O(n^2)
  // Brute force
  object Solution1 {
    def threeSum(A: Array[Int]): List[List[Int]] = {
      val map = scala.collection.mutable.HashMap[Int, List[Int]]()
      A.zipWithIndex foreach {case (x, i) => map.put(x, i::map.getOrElse(x, Nil))}
      var l = List[List[Int]]()
      for{
        i <- A.indices
        j <- i+1 until A.length
        t = -A(i)-A(j)
        if map contains t
        if !map(t).forall(x => x==i || x==j)
      } l ::= List(A(i), A(j), -A(i)-A(j)).sorted
      l.distinct
    }
  }

  // TLE:
  // Hash, sort and edge-cutting
  object Solution2 {
    def threeSum(A: Array[Int]): List[List[Int]] = {
      val map = scala.collection.mutable.HashMap[Int, List[Int]]()
      A.sorted.zipWithIndex foreach {case (x, i) => map.put(x, i::map.getOrElse(x, Nil))}

      def f(A:Array[Int])(i:Int, j:Int, acc:List[List[Int]] = Nil):List[List[Int]] = {
        if( i >= A.length || A(i) >0) acc
        else if (j >= A.length || A(i)+A(j) > 0) f(A)(i+1,i+2, acc)
        else {
          val n = -A(i)-A(j)
          if ((map contains n )&& (map(n) exists {x => x > (i max j)}))
            f(A)(i,j+1, List(A(i), A(j), n)::acc)
          else  f(A)(i,j+1, acc)
        }
      }

      f(A.sorted)(0,1).distinct
    }
  }

  // AC:
  // still tle on Leetcode_cn, got ac on leetcode.com
  // Runtime: 6564 ms, faster than 97.56% of Scala online submissions for 3Sum.
  // Memory Usage: 80.5 MB, less than 100.00% of Scala online submissions for 3Sum.
  // Take case for all zero case.
  object Solution {
    def threeSum(A: Array[Int]): List[List[Int]] = {
      val map = scala.collection.mutable.HashMap[Int,Int]()
      val B = A.sorted
      B.zipWithIndex foreach {case (x, i) => map.put(x, i)}

      def f(A:Array[Int])(i:Int, j:Int, acc:List[List[Int]] = Nil):List[List[Int]] = {
        if( i >= A.length || A(i) >0) acc
        else if (j >= A.length || A(i)+A(j) > 0) f(A)(i+1,i+2, acc)
        else {
          val n = -A(i)-A(j)
          if ((map contains n )&& (map(n) > (i max j))){
            val h = List(A(i), A(j), n)
            // skip j+1, j+2,.. which equal to A(j)
            if(j+1 < A.length) {
              val tmp = (j+1 until A.length).dropWhile(x =>A(x) == A(j)).headOption
              tmp match {
                case None => f(A)(i+1, i+2, h::acc)
                case Some(jj) => f(A)(i,jj, h::acc)
              }
            }
            else
              f(A)(i,j+1, h::acc)
          }
          else  f(A)(i,j+1, acc)
        }
      }

      f(B)(0,1).distinct
    }
  }




}
