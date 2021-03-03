package lab3

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class TweetSetSpec extends AnyFreeSpec with Matchers {

  val set1 = Empty
  val set2 = set1.add(new Tweet("a", "a body", 20))
  val set3 = set2.add(new Tweet("b", "b body", 20))
  val c = new Tweet("c", "c body", 7)
  val d = new Tweet("d", "d body", 9)
  val set4c = set3.add(c)
  val set4d = set3.add(d)
  val set5 = set4c.add(d)

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  "filter: on empty set" in {

    size(set1.filter(tw => tw.user == "a")) shouldBe 0

  }

  "filter: a on set5" in {

    size(set5.filter(tw => tw.user == "a")) shouldBe 1

  }

  "filter: 20 on set5" in {

    size(set5.filter(tw => tw.retweets == 20)) shouldBe 2

  }

  "union: set4c and set4d" in {

    size(set4c.union(set4d)) shouldBe 4

  }

  "union: with empty set (1)" in {

    size(set5.union(set1)) shouldBe 4

  }

  "union: with empty set (2)" in {

    size(set1.union(set5)) shouldBe 4

  }

  "descending: set5" in {
    val trends = set5.descendingByRetweet
    trends.isEmpty shouldBe false
    trends.head.user == "a" || trends.head.user == "b" shouldBe true
  }

}
