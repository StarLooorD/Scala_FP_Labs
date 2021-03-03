package lab3

import scala.io.Source
import scala.util.Using

object TweetLoader {
  lazy val allTweets: TweetSet = Using.resource(Source.fromResource("tweets.txt")) { it =>
    it.getLines()
      .map(_.split('|'))
      .map(a => new Tweet(a(0), a(1), a(2).toInt))
      .foldLeft(Empty: TweetSet)(_.add(_))
  }
}
