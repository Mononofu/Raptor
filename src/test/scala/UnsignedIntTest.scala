package Raptor

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import Constants.LongToUnsignedInt

class UnsignedIntSpec extends FlatSpec with ShouldMatchers {

  "An UnsignedInt" should "implement conversion from long correctly" in {
    val n: UnsignedInt = 978944421l.toInt
    n should equal (978944421)
  }

  it should "convert correctly from int" in {
    val n: UnsignedInt = 2329392
    n should equal (2329392)
  }

  it should "implement modulo correctly" in {
    val n: UnsignedInt = 256l * 78944421l + 121l
    (n % 256) should equal (121)
  }

  it should "behave correctly with XOR" in {
    val a: UnsignedInt = 2402805061l
    val b: UnsignedInt = 4016898363l

    (a ^ b) should equal (1616569470)
  }
}