package Raptor

import scala.io.Source

// do some magic to use unsigned int in scala
// not complete, just the minimum we need here
class UnsignedInt(protected val n: Int) {
	def %(other: Int) = n.%(other) + (if(n < 0) other else 0)
	def ^(other: UnsignedInt) = n.^(other.n)
	override def equals(that: Any) = that match {
		case other: Int => n == other
		case _ => false
	}
}

object Constants {
	implicit def LongToUnsignedInt(n: Long) = new UnsignedInt(n.toInt)
	implicit def IntToUnsignedInt(n: Int) = new UnsignedInt(n)

	// RFC 5053, 5.6.1: The Table V0 (used for the random number generator)
	val V0: List[UnsignedInt] = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("V0.constant")).getLines.map(s => new UnsignedInt(s.toLong.toInt)).toList

	// RFC 5053, 5.6.2: The Table V1
	val V1: List[UnsignedInt] = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("V1.constant")).getLines.map(s => new UnsignedInt(s.toLong.toInt)).toList

	// RFC 5053, 5.7: Systematic Indices J(K)
	val J: List[Int] = List(-1, -1, -1, -1) ++ Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("J.constant")).getLines.map(_.toInt).toList

	val Primes: List[Int] = Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("primes.constant")).getLines.map(_.toInt).toList

	val Factorials: List[BigInt] = List(BigInt(0)) ++ Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("factorial.constant")).getLines.map(s => BigInt(s)).toList

	val S: List[Int] = List(2) ++ Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("S.constant")).getLines.map(_.toInt).toList

	val H: List[Int] = List(2) ++ Source.fromInputStream(getClass.getClassLoader.getResourceAsStream("H.constant")).getLines.map(_.toInt).toList

	val Kmax = 8192
}