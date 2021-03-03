package lab1

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class BalanceSpec extends AnyFreeSpec with Matchers {

  private def bal(s: String) = Lab1.balance(s.toList)

  "'(a (0? b) max (/ 2 z))' збалансовано" in {
    bal("(a (0? b) max (/ 2 z))") shouldBe true
  }

  "'Bla bla (bla ...' збалансовано" in {
    bal("Bla bla (bla (bla) bla).\n(Bla bla blabla bla)") shouldBe true
  }

  "':-)' не збалансовано" in {
    bal(":-)") shouldBe false
  }

  "'())(' не збалансовано" in {
    bal("())(") shouldBe false
  }
}