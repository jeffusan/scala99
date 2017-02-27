package training

import scala.collection._

/**
  *
  *
  * Objective:
  *
  * Write a Java function, printTree(), which prints a given tree to stdout.  Details:
*The argument of printTree is a stream of Relations: pairs of "parent -> child" relationships.
*Each string found anywhere in the input represents a unique node.
*Each parent can have many children.
*The input list may contain Relations in any order, although:
*The order in which the pairs appear in the input list determines the nodes’ order with respect to its siblings.
 **
 *
  * Example input:
*List input = newArrayList();
 **
  * input.add(new Relation(“animal”, “mammal”));
*input.add(new Relation(“animal”, “bird”));
*input.add(new Relation(“lifeform”, “animal”));
*input.add(new Relation(“cat”, “lion”));
*input.add(new Relation(“mammal”, “cat”));
*input.add(new Relation(“animal”, “fish”));
 **
  * TreePrinter.printTree(input);
 **
  * Expected output:
 *line 1: lifeform
*line 2:   animal
*line 3:     mammal
*line 4:       cat
*line 5:         lion
*line 6:     bird
*line 7:     fish
 *
 *
 */


class TreePrinter {


  def buildTree(relations: List[Relation]): Node = {

    val root = relations.foldLeft(relations.head.parent) {
      (root, relation) =>
        if (relation.child == root) relation.parent
        else root
    }

    val animalToChilds = relations.foldLeft(new immutable.HashMap[String, List[String]]()) {
      (map, relation) =>
        val parentChild = map.getOrElse(relation.parent, List())
        val updatedChild = parentChild.::(relation.child)
        map + (relation.parent -> updatedChild)
    }

    val node = buildNode(animalToChilds, root)

    node
  }

  def buildNode(map: Map[String, List[String]], name: String): Node = {
    val childrenNames = map.getOrElse(name, List[String]())
    val childrenNodes = childrenNames.foldLeft(List[Node]()) {
      (list, childName) =>
        val node = buildNode(map, childName)
        list.::(node)
    }
    Node(name, childrenNodes)
  }

  def printTree(relations: List[Relation]): Unit = {
    val root = buildTree(relations)
    printNode(root)
  }


  def printNode(node: Node): Unit = {
    printNode(node, 0)

  }

  def printNode(node: Node, level: Int): Unit = {
    println(" " * level + node.name)
    node.child.foreach(printNode(_, level + 1))
  }


}

case class Node(name: String, child: List[Node])

case class Relation(parent: String, child: String)



object TreePrinter {
  def main(args: Array[String]) {
    val animals = new TreePrinter()

    val relations = List(Relation("a", "b"), Relation("b", "c"), Relation("b", "d"), Relation("0", "a"))

//    animals.buildTree(relations)

    animals.printTree(relations)

  }
}


