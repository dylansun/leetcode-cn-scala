/**
  * 设计一个支持 push，pop，top 操作，并能在常数时间内检索到最小元素的栈。

    push(x) -- 将元素 x 推入栈中。
    pop() -- 删除栈顶的元素。
    top() -- 获取栈顶元素。
    getMin() -- 检索栈中的最小元素。
    示例:

    MinStack minStack = new MinStack();
    minStack.push(-2);
    minStack.push(0);
    minStack.push(-3);
    minStack.getMin();   --> 返回 -3.
    minStack.pop();
    minStack.top();      --> 返回 0.
    minStack.getMin();   --> 返回 -2.
  */

class MinStack() {
    /** initialize your data structure here. */
    var sk = List[(Int, Int)]()
    def push(x: Int) = {
      sk = (x, x min getMin()) :: sk
    }
    def pop() = {
      sk = sk.tail
    }
    def top(): Int = sk.head._1
    def getMin(): Int = if(sk.isEmpty) Int.MaxValue else sk.head._2

}

/**
  * Your MinStack object will be instantiated and called as such:
  * var obj = new MinStack()
  * obj.push(x)
  * obj.pop()
  * var param_3 = obj.top()
  * var param_4 = obj.getMin()
  */