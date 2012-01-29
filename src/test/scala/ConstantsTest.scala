package Raptor

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

class ConstantsSpec extends FlatSpec with ShouldMatchers {

  "V0 and V1" should "have 256 elements" in {
    Constants.V0.length should equal (256)
    Constants.V1.length should equal (256)
  }

  "J" should "have 8193 elements" in {
    Constants.J.length should equal (8193)
  }

  "S" should "have 8193 elements" in {
    Constants.S.length should equal (8193)
  }

  "H" should "have 8193 elements" in {
    Constants.H.length should equal (8193)
  }

  "primes" should "have 10.000 elements" in {
  	Constants.Primes.length should equal (10000)
  }
}