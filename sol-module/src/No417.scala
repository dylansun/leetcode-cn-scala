/**
  * Created by lilisun on 5/9/19.
  */
object No417 {
  object merelyAC {
    def pacificAtlantic(A: Array[Array[Int]]): List[Array[Int]] = {
      if (A.length == 0) Nil else
        (f1(A) intersect f2(A)).distinct map {case(i,j) => Array(i,j)}
    }

    def nei(p:(Int,Int)):List[(Int,Int)] = p match {
      case (i,j) => List((i+1,j),(i-1,j), (i, j+1), (i,j-1))
    }
    def inBound(n:Int,m:Int)(p:(Int,Int)):Boolean = p match{
      case (i,j) => i >= 0 && j >= 0 && i < n && j < m
    }

    def f1(A: Array[Array[Int]]): List[(Int,Int)] = {
      val n = A.length
      val m = A(0).length
      val s1 = (for{i <- 0 until n} yield (i, 0)).toSet ++
        (for(j <- 0 until m) yield (0, j)).toSet
      val visited = Array.fill(n,m)(false)
      s1 foreach {case (i,j) => visited(i)(j) = true}
      def help(l:List[(Int,Int)], acc:List[(Int,Int)]):List[(Int,Int)] = l match {
        case Nil => acc
        case _ =>
          val delta_l = l flatMap nei filter inBound(n,m) filterNot {case(i,j) => visited(i)(j)} filter {case(i,j) => nei((i,j)).exists{case(x,y) => x < 0 || y < 0 || (acc.contains((x,y)) && A(i)(j) >= A(x)(y))}}
          delta_l foreach {case (i,j)  => visited(i)(j) = true}
          help(delta_l, delta_l ++ acc)
      }
      help(s1.toList, s1.toList)
    }

    def f2(A: Array[Array[Int]]): List[(Int,Int)] = {
      val n = A.length
      val m = A(0).length
      val s1 = (for{i <- 0 until n} yield (i, m-1)).toSet ++
        (for(j <- 0 until m) yield (n-1, j)).toSet
      val visited = Array.fill(n,m)(false)
      s1 foreach {case (i,j) => visited(i)(j) = true}
      def help(l:List[(Int,Int)], acc:List[(Int,Int)]):List[(Int,Int)] = l match {
        case Nil => acc
        case _ =>
          val delta_l = l flatMap nei filter inBound(n,m) filterNot {case(i,j) => visited(i)(j)} filter {case (i,j) => nei((i,j)).exists{case(x,y) => x >= n || y >= m || (acc.contains((x,y)) && A(i)(j) >= A(x)(y))}}
          delta_l foreach {case (i,j)  => visited(i)(j) = true}
          help(delta_l, delta_l ++ acc)
      }
      help(s1.toList, s1.toList)
    }
  }
  object simplyfyCode{
    def pacificAtlantic(A: Array[Array[Int]]): List[Array[Int]] = {
      if (A.length == 0) Nil else{
        val n = A.length
        val m = A(0).length
        (dfs(A)(genSea(n,m)) intersect dfs(A)(genLand(n,m))).distinct map {case(i,j) => Array(i,j)}
      }
    }
    def genLand(n:Int, m:Int):List[(Int,Int)] =
      (0 until n).toList.map{i => (i, m-1)} ++ (0 until m).toList.map{j => (n-1, j)}
    def genSea(n:Int, m:Int):List[(Int,Int)] =
      (0 until n).toList.map{i => (i,0)} ++ (0 until m).toList.map{j => (0, j)}
    def dfs(A: Array[Array[Int]])(init:List[(Int,Int)]) = {
      def help(l:List[(Int,Int)], acc:List[(Int,Int)]):List[(Int,Int)] = l match {
        case Nil => acc
        case _ =>
          var delta_l = l flatMap nei filter inBound(A.length,A(0).length) filterNot acc.contains
          delta_l = delta_l.distinct filter {case(i,j) => nei((i,j)).exists{case(x,y) => acc.contains((x,y)) && A(i)(j) >= A(x)(y)}}
          help(delta_l, delta_l ++ acc)
      }
      help(init, init)
    }
    def nei(p:(Int,Int)):List[(Int,Int)] = p match {
      case (i,j) => List((i+1,j),(i-1,j), (i, j+1), (i,j-1))
    }
    def inBound(n:Int,m:Int)(p:(Int,Int)):Boolean = p match{
      case (i,j) => i >= 0 && j >= 0 && i < n && j < m
    }
  }
  object symmetric{
    def pacificAtlantic(A: Array[Array[Int]]): List[Array[Int]] = {
      if (A.length == 0) Nil else{
        val n = A.length
        val m = A(0).length
        (dfs(A)(genSea(n,m)) intersect dfs(A)(genLand(n,m))).distinct map {case(i,j) => Array(i,j)}
      }
    }
    def genLand(n:Int, m:Int):List[(Int,Int)] =
    (0 until n).toList.map{i => (i, m-1)} ++ (0 until m).toList.map{j => (n-1, j)}
  }
  def rotate(A:Array[Array[Int]]):Array[Array[Int]] = {
    val B = Array.fill(A.length, A(0).length)(0)
    for{
      i <- 0 until A.length
      j <- 0 until A(0).length
    } B(i)(j) = A(A.length - i)(A(0).length - j)
    B
  }
  def genSea(n:Int, m:Int):List[(Int,Int)] =
    (0 until n).toList.map{i => (i,0)} ++ (0 until m).toList.map{j => (0, j)}

  def dfs(A: Array[Array[Int]])(init:List[(Int,Int)]) = {
    def help(l:List[(Int,Int)], acc:List[(Int,Int)]):List[(Int,Int)] = l match {
      case Nil => acc
      case _ =>
        var delta_l = l flatMap nei filter inBound(A.length,A(0).length) filterNot acc.contains
        delta_l = delta_l.distinct filter {case(i,j) => nei((i,j)).exists{case(x,y) => acc.contains((x,y)) && A(i)(j) >= A(x)(y)}}
        help(delta_l, delta_l ++ acc)
    }
    help(init, init)
  }
  def nei(p:(Int,Int)):List[(Int,Int)] = p match {
    case (i,j) => List((i+1,j),(i-1,j), (i, j+1), (i,j-1))
  }
  def inBound(n:Int,m:Int)(p:(Int,Int)):Boolean = p match{
    case (i,j) => i >= 0 && j >= 0 && i < n && j < m
  }
}
