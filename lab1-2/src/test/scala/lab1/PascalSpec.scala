package lab1

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class PascalSpec extends AnyFreeSpec with Matchers {

  import Lab1.pascal

  "col=0,row=2" in {
    pascal(0, 2) shouldBe 1
  }

  "col=1,row=2" in {
    pascal(1, 2) shouldBe 2
  }

  "col=1,row=3" in {
    pascal(1, 3) shouldBe 3
  }
}
