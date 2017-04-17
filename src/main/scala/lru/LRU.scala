package lru

/**
  * using a functional approach: LRUCache is immutable,
  * It's reconstructed on each get or set operations.
  * O(capacity) as each element in the cache is accessed on both operations
  */
object LRU {

  def get[K,V](lru: LRUCache[K,V], k: K):(LRUCache[K,V], Option[V]) = {
    lru.map.get(k) match {
      case None => (lru, None)
      case Some(n) =>
        // update each element to represent the new position
        val newMap = lru.map.foldRight(Map[K,Node[V]]()) {
          (kv,m) =>
            if (kv._2.position > n.position)
              m + (kv._1 -> Node(kv._2.value, kv._2.position -1))
            else if (kv._1 == k)
              m + (kv._1 -> Node(kv._2.value, lru.map.size -1))
            else
              m + (kv._1 -> kv._2)
        }
        // return value and updated lru
        (LRUCache(lru.capacity, newMap), Some(n.value))
    }
  }

  def set[K,V](lru: LRUCache[K,V], k: K, v: V): LRUCache[K,V] = {
    // is k already in the cache?
    //  if so, remove k from cache
    //  else is the cache full?
    //   if so remove oldest entry
    // add k v
    lru.map.get(k) match {
      case Some (_) => set(remove(lru, k), k, v)
      case None =>
        if(lru.map.size >= lru.capacity)
          set(remove(lru, findOldest(lru)), k, v)
        else
          LRUCache(lru.capacity, lru.map + (k -> Node(v, lru.map.size)))
    }
  }

  def findOldest[K,V](lru: LRUCache[K,V]): K = {
    lru.map.filter(_._2.position == 0).head._1
  }

  private def remove[K,V](lru: LRUCache[K,V], k: K): LRUCache[K,V] = {

    // we assume k is present
    val p = lru.map(k).position

    def reorder: (Map[K,Node[V]], (K, Node[V])) => Map[K,Node[V]] =
      (m, kv) =>
        if(kv._1 == k )
          // don't add k back
          m
        else if (kv._2.position > p )
          // decrease position of position more recent than the removed
          m + (kv._1 -> Node(kv._2.value, kv._2.position - 1))
        else
          // just add back oldest entries
          m + (kv._1 -> kv._2)

    val updated = lru.map.foldLeft(Map[K,Node[V]]())(reorder)

    LRUCache[K,V](lru.capacity, updated)
  }

  def main(args: Array[String]): Unit = {
    val l1 = LRUCache(3, Map('a' -> Node('A', 0)))
    val l2 = set(l1, 'b', 'B')
    val l3 = set(l2, 'c', 'C')
    val l4 = get(l3, 'a')
    val l5 = set(l4._1, 'd', 'D')
    val r = get(l5, 'a')
    println(r._2)


  }
}

case class LRUCache[K,V](capacity: Int, map: Map[K,Node[V]])
case class Node[V](value: V, position: Int)