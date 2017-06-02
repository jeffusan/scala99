package tries2

import scala.collection.mutable

class WordSuggestions2(dictionary: List[String]) {

  import WordSuggestions2._

  val root: Node = buildTree(dictionary)

  def search(in: String): List[String] = {
    //get Node representing last char of input
    //traverse that node to build all combinations

    @annotation.tailrec
    def get(root: Node, chars: List[Char]): Option[Node] = chars match {
      case head :: Nil => root.children.get(head)
      case head :: tail => root.children.get(head) match {
        case None => None
        case Some(n) => get(n, tail)
      }
    }

    val top = get(root, in.toCharArray.toList)

    def traverseRoot(root: Node, pre: String): List[String] = {

      def traverse(node: Node, pre: String): List[String] = {

        val current = pre + node.value

        if(node.children.isEmpty) {
          List[String](current)
        } else {
          val list = node.isWord match {
            case b if b => List[String](current)
            case _ => List[String]()
          }
          node.children.values.foldLeft(list) {
            (l, n) => l ::: traverse(n, current)
          }
        }
      }

      root.children.values.foldLeft(List[String]()) {
        (l, n) => l ::: traverse(n, pre)
      }
    }

    top match {
      case None => List[String]()
      case Some(n) => traverseRoot(n, in)
    }
  }

  def buildTree(dic: List[String]): Node = {

    val root = Node('*', NotAWord, mutable.HashMap[Char, Node]())

    def addWord(root: Node, word: String): Node = {

      @annotation.tailrec
      def insert(root: Node, chars: List[Char]): Unit = {
        chars match {
          case head :: Nil =>
            val maybeChildren = root.children.get(head).map(_.children)
            root.children.put(head, Node(head, IsWord, maybeChildren.getOrElse(mutable.Map[Char, Node]())))

          case head :: tail =>
            val child = root.children.getOrElseUpdate(head, Node(head, NotAWord, mutable.Map[Char, Node]()))
            insert(child, tail)
        }
      }
      insert(root, word.toCharArray.toList)
      root
    }

    dic.foldLeft(root) { (res, word) =>
      addWord(res, word)
    }
  }



}

case class Node(value: Char, isWord: Boolean, children: mutable.Map[Char, Node])

object WordSuggestions2 {

  val NotAWord = false
  val IsWord = true

  def main(args: Array[String]): Unit = {
    val words = List("cad", "cado", "cali", "bob", "balo", "calt", "california")
    val suggestions = new WordSuggestions2(words)
    printNode(suggestions.root, 0)
    println(suggestions.search("ca"))
  }

  def printNode(node: Node, level: Int): Unit = {
    println(" " * level + node.value)
    node.children.values.foreach(printNode(_, level + 1))
  }
}
