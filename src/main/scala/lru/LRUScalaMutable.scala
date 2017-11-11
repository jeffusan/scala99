package lru

class LRUScalaMutable[K >: Null, V>: Null](size: Int) {

  case class Node(key: K, value: V, var prev: Node, var next: Node)

  private var tail: Node = Node(null, null, null, null)
  private var head: Node = Node(null, null, null, null)

  private val map = new scala.collection.mutable.HashMap[K, Node]

  private var count: Int = 0

  def add(k: K, v: V): Unit = {

    if(count >= size) {
      remove(tail.key)
    }

    val n = Node(k,v, head, null)
    head.next = n
    head = n
    if(count == 0) tail = n
    count = count + 1
    map.put(k, n)
  }

  def get(k: K): Option[V] = {
    map.get(k).map { n ⇒
      remove(k)
      add(k, n.value)
      n.value
    }
  }

  def remove(k: K): Unit = {
    map.remove(k)
    tail = tail.next
    count = count - 1
  }


}


object LRUScalaMutable {
  def main(args: Array[String]): Unit = {
    val lru = new LRUScalaMutable[Integer, String](3)

    lru.add(2, "lol")
    lru.add(5, "bob")
    lru.add(0, "no")
    lru.add(1, "zero")

    println(lru.get(1))
    println(lru.get(2))
    println(lru.get(0))
    println(lru.get(5))
  }
}
