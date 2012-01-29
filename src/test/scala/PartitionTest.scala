package Raptor

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers


class PartitionSpec extends FlatSpec with ShouldMatchers {

  "The Partition function" should "split a block in equal sized sub-blocks" in {
    for(i <- 1 to 100) {
      for(j <- 1 to i) {
        val (il, is, jl, js) = Tools.Partition(i, j)
        (jl * il + js * is) should equal (i)
      }
    }
  }
}