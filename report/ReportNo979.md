979. 在二叉树中分配硬币

考虑 root, left right的硬币数, 最优的分配方案是:

nl: node number of left sub tree

nr: node number of right sub tree

cl: coin number of left sub tree

cr: coint number of right sub tree

我们只需要将(cl-nl)个硬币放到root, 如果为负,则从root获取那么多硬币 同理右侧子树, 这样我们产生的cost就是
abs(cl-nl) + abs(cr-nr)

这样我们得到左右子书都满足硬币数==节点数, 递归

