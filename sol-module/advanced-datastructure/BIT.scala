class BIT(A:Array[Int]){
    val nums = A
    val n = nums.length
    val BITree = Array.fill(n+1)(0)
    for{
        i <- 0 until n
    }{
        updateBIT(i, nums(i))
    }

    // update a single element
    def update(i:Int, x:Int):Unit = {
        val diff = x - nums(i)
        nums(i)= x
        updateBIT(i, diff)
    }

    // update BIT 
    def updateBIT(i:Int, x:Int):Unit = fUpdateBIT(i+1, x)
    def fUpdateBIT(i:Int, x:Int):Unit = {
        if(i <= n){
            BITree(i)= BITree(i) + x
            fUpdateBIT(i+(i & -i), x)
        }
    }

    def sum(i:Int):Int = fsum(i+1, 0)

    def fsum(i:Int, acc:Int):Int = {
     if(i > 0) fsum(i - (i & -i), acc + BITree(i))
     else acc
    }
    def sumRange(i:Int, j:Int):Int = sum(j) - sum(i-1)

}

object testBIT{
    def main(args:Array[String]):Unit = {
        val A = Array(2,1,1,3,2,3,4,5,6,7,8,9)
        val bitree = new BIT(A)
        println(bitree.BITree.toList)
    }
}
