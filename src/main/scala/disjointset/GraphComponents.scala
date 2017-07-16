package disjointset

import scala.annotation.tailrec
import scala.io.StdIn


object GraphComponents {

  case class State(n: Int, sizes: Map[Size, Set[Id]], min: Int, max: Int, map: Map[Int, Node])

  case class Node(id: Int, parent: Int, size: Int)

  case class Id(value: Int)
  case class Size(value: Int)

  type Add = (Int, Int, State) => State

  private val NoParent = -1
//  private val NoParent = Node(NoParentId, null, 0)
  private val Root = -10
//  private val Root = Node(RootId, null, 0)

  private def adder: Add = (x , y, state) => {

    val n = state.n
    if(x == y || x < 1 || x > n || y < n + 1 || y > 2 * n) state
    else {

      val nx = state.map.getOrElse(x, Node(x, NoParent, 1))
      val ny = state.map.getOrElse(y, Node(y, NoParent, 1))

      val (sizes, newMap) = (nx.parent, ny.parent) match {
        case (NoParent, NoParent) =>
          val m0 = state.map.updated(x, Node(x, Root, 2))
          val m1 = m0.updated(y, Node(y, x, 1))
          val s = Size(2)
          val i = Id(x)
          val newSizes = state.sizes.updated(s, state.sizes.getOrElse(s, Set(i)) + i)
          (newSizes, m1)
        case (px, NoParent) =>
          addNodeAndUpdateRoot(nx, ny, state.map, state.sizes)
        case (NoParent, py) =>
          addNodeAndUpdateRoot(ny, nx, state.map, state.sizes)
        case (pxId, pyId) =>
          val px = getParent(nx, state.map)
          val py = getParent(ny, state.map)
          if (px.size >= py.size) {
            merge(state.map, px, py, state.sizes)
          } else {
            merge(state.map, py, px, state.sizes)
          }
      }


      val min = sizes.keySet.map(_.value).min
      val max = Math.max(state.max, sizes.keySet.map(_.value).max)
      State(state.n, sizes, min, max, newMap)

    }
  }

  private def merge(map: Map[Int, Node],
                    root: Node,
                    node: Node,
                    sizes: Map[Size, Set[Id]]): (Map[Size, Set[Id]], Map[Int, Node]) = {
    val newRoot = Node(root.id, Root, root.size + node.size)
    val m0 = map.updated(root.id, newRoot)
    val m1 = m0.updated(node.id, Node(node.id, root.id, 1))

    val ns = Size(node.size)
    val nId = Id(node.id)
    val rSize = Size(root.size + node.size)
    val rId = Id(root.id)

    // remove the merged Id from sizes
    val sizes0 = removeSize(sizes, ns, nId)
    //add the root to sizes
    val sizes1 = updateSizes(sizes0, rSize, rId)
    //remove rootId from previous size
    val sizes2 = removeSize(sizes1, Size(root.size), rId)

    (sizes2, m1)
  }

  private def addNodeAndUpdateRoot(existingN: Node,
                                   newN: Node,
                                   map: Map[Int, Node],
                                   sizes: Map[Size, Set[Id]]): (Map[Size, Set[Id]], Map[Int, Node]) = {
    val p = getParent(existingN, map)
    val m0 = map.updated(newN.id, Node(newN.id, p.id, 1))
    val m1 = m0.updated(p.id, Node(p.id, Root, p.size +1))

    val ps = Size(p.size)
    val ns = Size(p.size +1)
    val i = Id(p.id)
    val sizes0 = updateSizes(sizes, ns, i)
    val sizes2 = removeSize(sizes0, ps, i)
    (sizes2, m1)
  }

  private def updateSizes(sizes: Map[Size, Set[Id]], size: Size, id: Id): Map[Size, Set[Id]] = {
    sizes.updated(size, sizes.getOrElse(size, Set(id)) + id)
  }

  private def removeSize(sizes: Map[Size, Set[Id]], size: Size, id: Id): Map[Size, Set[Id]] = {
    val sizes1 = sizes.updated(size, sizes.getOrElse(size, Set(id)) - id)
    if(sizes1.getOrElse(size, Set()).isEmpty) sizes1 - size else sizes1
  }

  private def getParent(n: Node, map: Map[Int, Node]): Node = {
    if(n.parent == Root) n
    else getParent(map(n.parent), map)
  }

  def build(n: Int, in: List[(Int, Int)]): State = {

    in.foldLeft(State(n, Map[Size, Set[Id]](), 2, 0, Map[Int, Node]())) {
      (s, e) => {
        adder(e._1, e._2, s)
      }
    }

  }



  def main(args: Array[String]): Unit = {
    val in = read
    val r = build(in._1, in._2)
    println(r.min + " " + r.max)
  }

  def read: (Int, List[(Int, Int)]) = {

    val size = StdIn.readLine().toInt

    @tailrec
    def reread(i: Int, list: List[(Int, Int)]): List[(Int, Int)] = {
      val s = Option(StdIn.readLine())
      if (s.isEmpty) list else {
        val intsStr = s.get.split(' ')
        val ints = intsStr.map(_.toInt)
        reread(i + 1, list :+ (ints.head, ints(1)))
      }
    }

    val v = reread(0, List())
    (size, v)
  }
}

