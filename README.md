# Scala Solution for Leetcode

#### No.996 Number of Squareful Arrays
1. 构造graph 
2. dfs

注意事项: 
1. 不能直接保存路径会超时
2. 数组中有重复元素,需要计算每个元素的个数, 不能直接用idx来做 不然会超时 且超内存
3. scala中定义全局变量 每一次调用numSquarefulPerms函数 必须clear掉, 不然会冲突
4. 编程的代码还需要改进, 尽可能不适用全局变量, 不符合函数式编程。很容易出bug

#### No 995 Minimum Number of K Consecutive Bit Flips
1. 贪心算法。从左到右边: 遇到1,skip; 遇到0, 翻转。

```Scala
  object No995 {
      def minKBitFlips(A: Array[Int], K: Int): Int = minKBitFlips(A,K,0)
      def minKBitFlips(A: Array[Int], K: Int, acc: Int): Int = {
       // println(s"A: ${A.mkString("[", ",", "]")}, acc: $acc")
        if(K == A.length) {
          if(A.mkString == A.map(x => 1).mkString)  acc
          else if(A.mkString == A.map(x => 0).mkString)  1+acc
          else -1
        }else{
          if(A(0) == 1) minKBitFlips(A.tail, K, acc)
          else minKBitFlips(flip(A.tail, K), K, acc + 1)
        }
      }
    
      def flip(A: Array[Int], K: Int): Array[Int] = {
        val B = A
        for(x <- 0 until K - 1) B(x) = if(B(x) == 1) 0 else 1
        B
      }
  }
```

2.优化: 修改` minKBitFlips(flip(A.tail, K), K, acc + 1)`  直接进行翻转操作会TLE, 故只需要记录翻转次数,但是记录翻转次数和直接进行翻转的时间代价是一样的。
所以, 我们必须再次根据翻转操作的特殊性来减少时间开销。

翻转分析: 如果A(0) == 0, 那么翻转的范围是A(1),...,A(K-1),
接下来需要判断 A(1) 是否为 1。

在不翻转数组的情况,需要记录当前的翻转状态flap。

举一个特例: (0,0,0,1,0,0,0), k=3

起始状态: flag =0, 

对第一个元素进行操作, flag = 1 
 
第二个元素 为0 , 这是flag = 1, 表明已经翻转过了, 所以 0 不需要翻转

当到达地第4个元素为1的时候, 我们发现这时候, 需要额外的信息告诉我们 第4个点没有被翻转过,

这时候需要记录h(3) = 1, 并利用h(3) 来重置 flag 。

么重置之后, flag 变为 0, A(3)=1, 则不需要翻转。 那么重复这些步骤即可。

```Scala
  def minKBitFlips(A: Array[Int], K: Int): Int = solverOptGreedy(A, K)

  def solverOptGreedy(A: Array[Int], K: Int): Int = {
    val N = A.length
    val hint = (0 until N).map(x => 0).toArray
    var ans = 0; var flip = 0
    
    for (i <- 0 until N) {
      flip ^= hint(i)
      if (A(i) == flip) {  
        ans+= 1  
        if (i + K > N) return -1 
        flip ^= 1
        if (i + K < N) hint(i + K) ^= 1
      }
    }
    ans
  }
```

#### No. 994 Rotting Oranges

1. 寻找要变化的橘子的坐标
2. 如果存在 修改橘子
3. 如果不存在, 检查是否全部为rotten

```Scala
object Solution {
  def orangesRotting(grid: Array[Array[Int]]): Int = solver(grid, 0)

  def solver(grid: Array[Array[Int]], acc: Int): Int = {
    //Step 1
    val tbr = (for(x <- grid.indices) yield {
      (for(y <- grid(0).indices if grid(x)(y) == 1 && isAdjRot( (x, y), grid)) yield (x, y)).toList
    }).toList.reduce(_:::_)

    //Step 2 and 3
    if(tbr.isEmpty){
      if(grid.map(x => x.contains(1)).reduce(_||_)) -1 else acc
    }else{
      tbr.foreach( x => grid(x._1)(x._2) = 2)
      solver(grid, acc + 1)
    }
  }

  def isAdjRot(pos: (Int, Int), grid: Array[Array[Int]]): Boolean = {
    val offset = Array((1,0), (0,1), (-1, 0), (0, -1))
    (for(x <- offset ) yield{
      val np = (pos._1 + x._1, pos._2 + x._2)
      if(isInbound(np, grid)) grid(np._1)(np._2) == 2 else false
    }).reduce(_||_)
  }

  def isInbound(np: (Int, Int), grid: Array[Array[Int]]): Boolean = {
    np._1 >= 0 && np._2 >= 0 && np._1 < grid.length && np._2 < grid(0).length
  }
}
```



#### No.406 Queue Reconstruction by Height

1. 对(h,k) 按照h降序、k升序, 进行排序

2. 按照k依次插入到数列

Ex. 

    [[7,0], [4,4], [7,1], [5,0], [6,1], [5,2]] 

    [[7,0], [7,1], [6,1], [5,0], [5,2], [4,4]] // sort

     [7,0]
     [7,0],[7,1]
     [7,0],[6,1],[7,1]
     [5,0],[7,0],[6,1],[7,1]
     [5,0],[7,0],[5,2],[6,1],[7,1]
     [5,0],[7,0],[5,2],[6,1],[4,4],[7,1]
     


  
  

