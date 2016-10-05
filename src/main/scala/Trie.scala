
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.JavaConverters._

object Trie {
  def apply(): Trie = new TrieNode()
}

sealed trait Trie extends Traversable[String] {
  def append(word: String)
  def findPrefix(key: String): Option[String]
}

private class TrieNode(
  val char: Option[Char] = None,
  var word: Option[String] = None) extends Trie {

  private val childeren: mutable.Map[Char, TrieNode] = new java.util.TreeMap[Char, TrieNode].asScala

  override def append(word: String) = {
    @tailrec def appendHelper(node: TrieNode, index: Int): Unit = {
      if(index == word.length) {
        node.word = Some(word)
      } else {
        val char = word.charAt(index)
        var res = node.childeren.getOrElseUpdate(char, { new TrieNode(Some(char)) } )
        appendHelper(res,index + 1)
      }
    }
    appendHelper(this,0)
  }

  override def findPrefix(key: String): Option[String] = {
    @tailrec def findPrefixHelper(index: Int, node: TrieNode, lastWord: Option[String]): Option[String] = {
      if(node.childeren.isEmpty || index == key.length) {
        node.word.orElse(lastWord)
      } else {
        node.childeren.get(key.charAt(index)) match {
          case Some(child) => findPrefixHelper(index + 1, child, node.word.orElse(lastWord))
          case None => node.word.orElse(lastWord)
        }
      }
    }
    findPrefixHelper(0,this,None)
  }

  override def foreach[U](f: String => U): Unit = {
    @tailrec def foreachHelper(nodes: TrieNode*): Unit = {
      if(nodes.size != 0) {
        nodes.foreach(node => node.word.foreach(f))
        foreachHelper(nodes.flatMap(node => node.childeren.values) : _*)
      }
    }
    foreachHelper(this)
  }

  override def toString(): String = s"Trie(char=${char},word=${word})"
}
