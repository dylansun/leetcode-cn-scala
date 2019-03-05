
#### No.996 Number of Squareful Arrays
1. 构造graph 
2. dfs

注意事项: 
1. 不能直接保存路径会超时
2. 数组中有重复元素,需要计算每个元素的个数, 不能直接用idx来做 不然会超时 且超内存
3. scala中定义全局变量 每一次调用numSquarefulPerms函数 必须clear掉, 不然会冲突
4. 编程的代码还需要改进, 尽可能不适用全局变量, 不符合函数式编程。很容易出bug


