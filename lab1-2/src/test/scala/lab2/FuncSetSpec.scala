package lab2

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class FuncSetSpec extends AnyFreeSpec with Matchers {
  import Lab2._

  val s1 = singletonSet(1)
  val s2 = singletonSet(2)
  val s3 = singletonSet(3)

  def rangeSet(a: Int, b: Int): Set = x => x >= a && x <= b

  "contains" in {
    contains(_ => true, 100) shouldBe true
  }

  "singletonSet" in {
    contains(s1, 1) shouldBe true
  }

  "union" in {
    val s = union(s1, s2)

    contains(s, 1) shouldBe true
    contains(s, 2) shouldBe true
    contains(s, 3) shouldBe false
  }

  "intersect" in {
    val s = intersect(rangeSet(1, 2), rangeSet(2, 3))
    contains(s, 1) shouldBe false
    contains(s, 2) shouldBe true
    contains(s, 3) shouldBe false
  }

  "diff" in {
    val s = diff(rangeSet(1, 2), rangeSet(2, 3))
    contains(s, 1) shouldBe true
    contains(s, 2) shouldBe false
    contains(s, 3) shouldBe false
  }

  "filter" in {
    val s = filter(rangeSet(1, 4), _ % 2 == 0)
    contains(s, 1) shouldBe false
    contains(s, 2) shouldBe true
    contains(s, 3) shouldBe false
    contains(s, 4) shouldBe true
  }

  "forall" in {
    val s = rangeSet(1, 4)

    forall(s, _ > 0) shouldBe true
    forall(s, _ % 2 == 0) shouldBe false
  }

  "exists" in {
    val s = rangeSet(1, 4)

    exists(s, _ > 0) shouldBe true
    exists(s, _ < 0) shouldBe false
    exists(s, _ % 2 == 0) shouldBe true
  }

  "map" in {
    val s = map(rangeSet(1, 4), _ * 2)
    makeString(s) shouldBe "{2, 4, 6, 8}"
  }
}
