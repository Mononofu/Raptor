package Raptor

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers


class RandSpec extends FlatSpec with ShouldMatchers {

  "The PRNG Rand" should "be stateless" in {
    Tools.Rand(100, 232, 32) should equal (Tools.Rand(100, 232, 32))
    Tools.Rand(185, 97, 1232) should equal (Tools.Rand(185, 97, 1232))
    Tools.Rand(11112, 9778, 33434) should equal (Tools.Rand(11112, 9778, 33434))
  }

  it should "return a number in the correct range" in {
    for(i <- 1 to 1000) {
      Tools.Rand(100, 232, i) should be < (100)
    }
  }
}