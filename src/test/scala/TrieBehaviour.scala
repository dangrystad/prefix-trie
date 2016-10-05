import org.scalatest._

class TrieBehaviour extends FlatSpec with Matchers {

  "A Trie" should "append words" in {
    val t = new TrieNode()
    t.append("HolyCow")
    t.foreach(println(_))
  }

  "A Trie" should "find prefixes" in {
    val t = new TrieNode()
    t.append("0")
    t.append("012")
    t.append("01234")
    t.findPrefix("0") should be (Some("0"))
    t.findPrefix("012") should be (Some("012"))
    t.findPrefix("0123") should be (Some("012"))
    t.findPrefix("01234") should be (Some("01234"))
    t.findPrefix("012345") should be (Some("01234"))
    t.findPrefix("1") should be (None)
    t.findPrefix("") should be (None)
  }

  "A Trie" should "handle tree structures" in {
    val t = new TrieNode()
    t.append("111")
    t.append("122")
    t.append("133")
    t.append("222")
    t.append("333")
    t.findPrefix("111123") should be (Some("111"))
    t.findPrefix("122123") should be (Some("122"))
    t.findPrefix("1223") should be (Some("122"))
    t.findPrefix("999") should be (None)
    t.findPrefix("") should be (None)
  }
}
