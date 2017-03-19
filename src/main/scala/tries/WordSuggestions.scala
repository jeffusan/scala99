package tries

import scala.collection.mutable

class WordSuggestions(dictionary: List[String]) {

  import WordSuggestions._

  val root: Node = buildTree(dictionary)


  def search(in: String): List[String] = {
    // get node corresponding to last char of input + words found on the way
    // build all words based on current input + all children
    val chars = in.toCharArray.toList



    @annotation.tailrec
    def getInputNode(chars: List[Char], root: Node): Option[Node] = chars match {
      case head :: Nil => root.children.get(chars.head)
      case head :: tail =>
        root.children.get(head) match {
          case None => None
          case Some(n) => getInputNode(tail, n)
        }
    }

    def getWords(root: Node): List[String] = {

      def traverse(n: Node, pre: String): List[String] = {

        val current = pre + n.value
        if (n.children.isEmpty) {
          List(current)
        } else {
          val words = pre match {
            case c if n.isWord => List(pre + n.value)
            case _ => List[String]()
          }

          n.children.values.foldLeft(words) {
            (res, child) => res ::: traverse(child, current)
          }
        }
      }

      root.children.values.foldLeft(List[String]()) {
        (res, child) => res ::: traverse(child, "")
      }
    }

    getInputNode(chars, root) match {
      case None => List()
      case Some(r) => getWords(r).map(in + _)
    }
  }


  def buildTree(dictionary: List[String]): Node = {
    dictionary.foldLeft(Node('*', NotAWord, mutable.Map())) {
      (res, word) => addWord(res, word)
    }
  }


  def addWord(tree: Node, w: String): Node = {

    val chars = w.toCharArray.toList

    def insert(n: Node, values: List[Char]): Unit = {
      values match {
        case c :: Nil =>
          //get current children of c node
          val maybeMap = n.children.get(c).map(_.children)
          //replace c node to mark is as word and give it the children it might have had before
          n.children.put(c, Node(c, IsWord, maybeMap.getOrElse(mutable.Map())))
          //end as it was the end of the word to be inserted
        case head :: tail =>
          val child = n.children.getOrElseUpdate(head, Node(head, NotAWord, mutable.Map()))
          insert(child, tail)
      }
    }

    insert(tree, chars)
    tree
  }




}

case class Node(value: Char, isWord: Boolean, children: mutable.Map[Char, Node])

object WordSuggestions {

  val NotAWord = false
  val IsWord = true

  def main(args: Array[String]): Unit = {
    val words = List("cad", "cado", "cali", "bob", "balo", "calt", "california")
    val suggestions = new WordSuggestions(words)
    println(suggestions.search("ca"))
//    println(suggestions.search("ca"))
  }

  def printNode(node: Node, level: Int): Unit = {
    println(" " * level + node.value)
    node.children.values.foreach(printNode(_, level + 1))
  }
}
