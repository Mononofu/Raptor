package Raptor

case class Symbol(n: Int) {
	// TODO: implement this operator
	def ^(that: Symbol) = this
}

object Tools {
	// RFC 5053, 5.4.4.1: Random Generator
	// produces a random number between 0 and m-1 inclusive
	def rand(X: Int, i: Int, m: Int) = (Constants.V0( (X + i) % 256 ) ^ Constants.V1( ((X/256.).toInt + i) % 256)) % m

	// RFC 5053, 5.4.4.2: Degree Generator
	def deg(v: Int) = v match {
		case n if n < 10241 => 1
		case n if n < 491582 => 2
		case n if n < 712794 => 3
		case n if n < 831695 =>  4
		case n if n < 948446 => 10
		case n if n < 1032189 => 11
		case n if n < 1048576 => 40
		case _ => 0
	}

	// RFC 5053, 5.4.4.3: LT Encoding Symbol Generator
	def LTEnc(K: Int, C: List[Symbol], triple: (Int, Int, Int)) = {
		val d = triple._1
		val a = triple._2
		var b = triple._3

		val X = getX(K)
		val S = getS(K, X)
		val H = getH(K, S)

		val L = K+S+H
		val Lprime = smallestPrimeGreaterEqualThan(L)

		while(b >= L)
			b = (b + a) % Lprime
		
		var result = C(b)

		for(j <- 1 to math.min(d-1, L-1)) {
			b = (b + a) % Lprime 
			while(b >= L)
				b = (b + a) % Lprime
			result = result ^ C(b)
		}
		result
	}

	def getX(K: Int) = math.ceil(0.5 * (math.sqrt(8*K + 1) + 1)).toInt
	def getS(K: Int, X: Int) = {
		if(K <= Constants.Kmax) Constants.S(K)
		else smallestPrimeGreaterEqualThan(math.ceil(0.01*K).toInt + X)
	}
	def getH(K: Int, S: Int) = {
		if(K <= Constants.Kmax) Constants.H(K)
		else (2 to 99).filter(H => choose(H, math.ceil(H/2.).toInt) >= K + S).head
	}

	def choose(i: Int, j: Int) = binomialCoefficient(i, j)
	def binomialCoefficient(n:Int, k:Int): BigInt = fact(n) / (fact(k) * fact(n-k))
   	def fact(n: Int): BigInt = Constants.Factorials(n)
   	def smallestPrimeGreaterEqualThan(n: Int): Int = Constants.Primes.filter(_ >= n).head
   	def largestPrimeSmallerThan(n: Int): Int = Constants.Primes.filter(_ < n).last


}