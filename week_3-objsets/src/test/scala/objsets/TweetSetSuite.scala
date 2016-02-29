package objsets

import java.util.NoSuchElementException

import jdk.nashorn.internal.runtime.regexp.joni.exception.ValueException
import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class TweetSetSuite extends FunSuite {
  trait TestSets {
    val set1 = new Empty
    val set2 = set1.incl(new Tweet("a", "a body", 20))
    val set3 = set2.incl(new Tweet("b", "b body", 20))
    val c = new Tweet("c", "c body", 7)
    val d = new Tweet("d", "d body", 9)
    val set4c = set3.incl(c)
    val set4d = set3.incl(d)
    val set5 = set4c.incl(d)
  }

  def asSet(tweets: TweetSet): Set[Tweet] = {
    var res = Set[Tweet]()
    tweets.foreach(res += _)
    res
  }

  def size(set: TweetSet): Int = asSet(set).size

  test("filter: on empty set") {
    new TestSets {
      assert(size(set1.filter(tw => tw.user == "a")) === 0)
    }
  }

  test("filter: a on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.user == "a")) === 1)
    }
  }

  test("filter: 20 on set5") {
    new TestSets {
      assert(size(set5.filter(tw => tw.retweets == 20)) === 2)
    }
  }

  test("union: set4c and set4d") {
    new TestSets {
      assert(size(set4c.union(set4d)) === 4)
    }
  }

  test("union: with empty set (1)") {
    new TestSets {
      assert(size(set5.union(set1)) === 4)
    }
  }

  test("union: with empty set (2)") {
    new TestSets {
      assert(size(set1.union(set5)) === 4)
    }
  }



  test("biggest tweet: set 5") {
    new TestSets {
      assert(set5.mostRetweeted.user == "a" || set5.mostRetweeted.user == "b")
    }
  }

  test("biggest tweet: c and d") {
    new TestSets {
      assert((new Empty).incl(c).incl(d).mostRetweeted.user == "d")
      assert((new Empty).incl(d).incl(c).mostRetweeted.user == "d")
    }
  }

  test("finding biggest tweet of an emptyset throws a NoSuchElementException") {
    intercept[NoSuchElementException] {
      (new Empty).mostRetweeted
    }
  }

  test("finding biggest tweet with negative retweets throws a ValueException") {
    val neg_tweets = (new Empty).incl(new Tweet("neg", "neg body", -1))
    intercept[ValueException] {
      neg_tweets.mostRetweeted
    }
  }

  test("most retweeted can have 0 retweets") {
    val zero_tweet = (new Empty).incl(new Tweet("zero", "zero body", 0))
    assert(zero_tweet.mostRetweeted.retweets == 0)
  }


  test("descending: set5") {
    new TestSets {
      val trends = set5.descendingByRetweet
      val trends_less_one = trends.tail
      val trends_less_two = trends_less_one.tail
      val trends_less_three = trends_less_two.tail

      assert(!trends.isEmpty)
      assert(trends.head.user == "a" || trends.head.user == "b")
      assert(trends_less_one.head.user == "a" || trends_less_one.head.user == "b")
      assert(trends_less_two.head.user == "d")
      assert(trends_less_three.head.user == "c")
      assert(trends_less_three.tail.isEmpty)
    }
  }

  test("descending of empty is empty") {
    assert((new Empty).descendingByRetweet.isEmpty)
  }

  }
