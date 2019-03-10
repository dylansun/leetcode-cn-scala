/**
  * Created by lilisun on 3/11/19.
  */
object No707 {
  class MyLinkedList() {

    /** Initialize your data structure here. */

    var linkerList = Array[Int]()
    /** Get the value of the index-th node in the linked list. If the index is invalid, return -1. */
    def get(index: Int): Int = {
      if(linkerList.indices.contains(index)) linkerList(index) else -1
    }

    /** Add a node of value val before the first element of the linked list. After the insertion, the new node will be the first node of the linked list. */
    def addAtHead(value: Int) {
      linkerList = Array(value) ++ linkerList
    }

    /** Append a node of value val to the last element of the linked list. */
    def addAtTail(value: Int) {
      linkerList = linkerList ++ Array(value)
    }

    /** Add a node of value val before the index-th node in the linked list. If index equals to the length of linked list, the node will be appended to the end of linked list. If index is greater than the length, the node will not be inserted. */
    def addAtIndex(index: Int, value: Int) {
      if((0 to linkerList.length).contains(index))
      linkerList = linkerList.slice(0,index) ++ Array(value) ++ linkerList.slice(index, linkerList.length)
    }

    /** Delete the index-th node in the linked list, if the index is valid. */
    def deleteAtIndex(index: Int) {
      if(linkerList.indices.contains(index))
      linkerList = linkerList.slice(0,index)  ++ linkerList.slice(index+1, linkerList.length)
    }

  }

  /**
    * Your MyLinkedList object will be instantiated and called as such:
    * var obj = new MyLinkedList()
    * var param_1 = obj.get(index)
    * obj.addAtHead(`val`)
    * obj.addAtTail(`val`)
    * obj.addAtIndex(index,`val`)
    * obj.deleteAtIndex(index)
    */
}
