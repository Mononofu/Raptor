package Raptor

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers


import scala.io.Source
class RelationshipSpec extends FlatSpec with ShouldMatchers {

  "X" should "be the smallest positive integer such that X*(X-1) >= 2*K" in {
    for(K <- 1 to Constants.Kmax) {
      val X = Tools.getX(K) 
      (X*(X-1)) should be >= (2*K)
      ((X-1)*(X-2)) should be < (2*K)
    }
  }
 
  "S" should "be the smallest prime such that S >= ceil(0.01*K) + X" in {
    for(K <- 1 to Constants.Kmax) {
      val X = Tools.getX(K)
      val S = Tools.getS(K, X)
      S should be >= (math.ceil(0.01*K).toInt + X)
      Tools.largestPrimeSmallerThan(S) should be < (math.ceil(0.01*K).toInt + X)
    }
  }

  "H" should "be the smallest integer such that choose(H,ceil(H/2)) >= K + S" in {
    for(K <- 1 to Constants.Kmax) {
      val X = Tools.getX(K)
      val S = Tools.getS(K, X)
      val H = Tools.getH(K, S)
      Tools.choose(H, math.ceil(H/2.).toInt).toInt should be >= (K + S)
      Tools.choose(H-1, math.ceil((H-1)/2.).toInt).toInt should be < (K + S)
    }
  }


}