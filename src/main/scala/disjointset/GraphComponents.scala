package disjointset



object GraphComponents {

  case class State(sizes: Map[Size, Set[Id]], min: Int, max: Int, map: Map[Int, Node])

  case class Node(id: Int, parent: Int, size: Int)

  case class Id(value: Int)
  case class Size(value: Int)

  type Add = (Int, Int, State) => State

  private val NoParent = -1

  private def adder: Add = (x , y, state) => {

    val nx = state.map.getOrElse(x, Node(x, NoParent, 1))
    val ny = state.map.getOrElse(y, Node(y, NoParent, 1))

    val (sizes, newMap) = (nx.parent, ny.parent) match {
      case (NoParent, NoParent) =>
        val m0 = state.map.updated(x, Node(x, x, 2))
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
        val px = getParent(state.map.getOrElse(pxId, nx), state.map)
        val py = getParent(state.map.getOrElse(pyId, ny), state.map)
        if (px.size >= py.size) {
          merge(state.map, x, px, y, py, state.sizes)
        } else {
          merge(state.map, y, py, x, px, state.sizes)
        }
    }


    val min = sizes.keySet.map(_.value).min
    val max = Math.max(state.max, sizes.keySet.map(_.value).max)
    State(sizes, min, max, newMap)

  }

  private def merge(map: Map[Int, Node],
                    rootValue: Int,
                    root: Node,
                    nValue: Int,
                    nParent: Node,
                    sizes: Map[Size, Set[Id]]): (Map[Size, Set[Id]], Map[Int, Node]) = {
    val m0 = map.updated(rootValue, Node(rootValue, root.id, root.size + 1))
    val m1 = m0.updated(nValue, Node(nValue, root.id, 1))

    val ns = Size(nParent.size)
    val nId = Id(nParent.id)
    val rSize = Size(root.size + nParent.size)
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
    val p = getParent(map.getOrElse(existingN.parent, existingN), map)
    val m0 = map.updated(newN.id, Node(newN.id, p.id, 1))
    val m1 = m0.updated(p.id, Node(p.id, p.id, p.size +1))

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
    if(sizes1.getOrElse(size, Map()).isEmpty) sizes1 - size else sizes1
  }

  private def getParent(n: Node, map: Map[Int, Node]): Node = {
    if(n.parent == n.id) n
    else getParent(map.getOrElse(n.parent, n), map)
  }

  def build(in: List[(Int, Int)]): State = {

    in.foldLeft(State(Map(), 2, 0, Map[Int, Node]())) {
      (s, e) => {
        adder(e._1, e._2, s)
      }
    }

  }
}

