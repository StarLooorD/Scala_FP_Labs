package lab1

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class CountChangeSpec extends AnyFreeSpec with Matchers {

  import Lab1.countChange

  "простий приклад" in {
    countChange(4, List(1, 2)) shouldBe  3
  }

  "посортовані монети" in {
    countChange(300, List(5, 10, 20, 50, 100, 200, 500)) shouldBe 1022
  }

  "без необхідної монети" in {
    countChange(301, List(5, 10, 20, 50, 100, 200, 500)) shouldBe 0
  }

  "непосортовані монети" in {
    countChange(300, List(500, 5, 50, 100, 20, 200, 10)) shouldBe 1022
  }

  "без монет" in {
    countChange(300, List()) shouldBe 0
  }

  "без грошей" in {
    countChange(0, List(500, 5, 50, 100, 20, 200, 10)) shouldBe 1
  }
}