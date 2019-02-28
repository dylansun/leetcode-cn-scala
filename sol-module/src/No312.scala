object No312 {
    def maxCoins(nums: Array[Int]): Int = dp_solver(nums)

    def dp_solver(nums: Array[Int]): Int = {
    if (nums.length == 0) return 0
    if (nums.length == 1) return nums(0)
    val nlen = nums.length + 2
    val coins:Array[Int] = Array(1) ++ nums ++ Array(1)
    val dp = Array.ofDim[Int](nlen,nlen)
    for (len <- 2 until nlen) {
      for (l <- 0 until nlen - len) {
        val r = l + len
        for ( k <- l + 1 until r) {
          dp(l)(r) = dp(l)(r) max dp(l)(k) + coins(l) * coins(k) * coins(r) + dp(k)(r)
        }
      }
    }
    dp(0)(nlen-1)
    }


    def maxCoins_bf_hash(nums: Array[Int]): Int = {
      val time1=System.currentTimeMillis()
      val mem = scala.collection.mutable.HashMap[Int, Int]()
      mem.put(0,0)
      val l: List[Int] = nums.indices.map(x => Math.pow(2, x).toInt ^ (Math.pow(2, nums.length) -1).toInt ).toList

      val table = genIndex(nums.length, List[String](""))
      for(x <- nums.indices) mem.put(Math.pow(2,x).toInt, nums(x)) // 3,1,5,8 => 0 1 ,2,,3 => 1 2 4 8

      //循环顺序不对,不能按数字序
      //按照1的数量来排序
      //println(mem)
      val loop = (1 until Math.pow(2,nums.length).toInt).map(x => (count1(x,0), x)).sortBy(_._1).map(_._2)
      val time2=System.currentTimeMillis()
      for(x <- loop if !mem.contains(x)){
        val candidate = for(y <- l if (x & y) != x && (x & y)!= 0)yield{
          val t = x & y

          /*val a1 = table(x)
          val a2 = table(y)
          println(s"x: $a1, y: $a2, x:$x, t: $t")*/
          val yi = table(y).reverse.indexOf('0')
          val xil = table(x).reverse.zipWithIndex.filter(_._1 == '1').map(_._2).toList
          val t_l = xil.filter(_ < yi)
          val t_r = xil.filter(_ > yi)
          val v_t_l = if(t_l.nonEmpty) nums(t_l.max) else 1
          val v_t_r = if(t_r.nonEmpty) nums(t_r.min) else 1
          //println(s"left value: $v_t_l right value: $v_t_r")
          //println(s"yi: $yi xil: $xil, t_l: $t_l t_r: $t_r")
          nums(yi)*v_t_l*v_t_r + mem(t)
        }
       // println(candidate)
       // println(s"x: $x ${table(x)}, mem(x) = ${candidate.max}")
       // println(mem)
        mem.put(x, candidate.max)
      }
      val time3=System.currentTimeMillis()
      println(s"Time1: ${time2-time1} t2: ${time3-time2}")
      mem(Math.pow(2,nums.length).toInt-1)
    }

  def count1(n: Int, acc: Int):Int = n match {
    case 0 => acc
    case _ => if(n %2 == 1) count1(n /2, acc+1) else count1(n /2, acc)
  }

  def genIndex(n: Int, acc: List[String]): List[String] = n match {
    case 0 => acc
    case _ => {
      var ans = List[String]()
      for (x <- acc) for (y <- List("0", "1")) ans = ans ::: List(x + y)
      genIndex(n - 1, ans)
    }
  }


    def maxCoins_bf(nums: Array[Int]): Int = {
      if(nums.length == 0) return 0
      if(nums.length == 1) return nums(0)
      val n = nums.length
      (for(x <- nums.indices) yield{
        x match {
          case 0  => nums(0)*nums(1)+maxCoins(nums.drop(1))
          case i: Int if i == n-1 =>  nums(n-1)*nums(n-2) + maxCoins(nums.dropRight(1))
          case _ =>  nums(x-1)*nums(x)*nums(x+1) + maxCoins(nums.slice(0,x) ++ nums.slice(x+1, n))
        }
      }).max
    }



  def main(args: Array[String]): Unit = {

    //println(maxCoins(Array(7,9,8,0,7,1,3,5,5,2,3)))
    val nums = Array(7,9,8,0,7,1,3,5,5,2,3)
    val nums2 = Array(3,1,5,8)
    val nums3 = Array(2,4,8,4,0,7,8,9,1,2,4,7,1,7,3)
    println(maxCoins(nums2))

   //  val x = 1 to 100
   //  println(x.map( x => (count1(x, 0), x)).sortBy( x => x._1))
  }
}
