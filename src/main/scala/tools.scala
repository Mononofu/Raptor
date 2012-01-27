package Raptor

object Tools {
	// RFC 5053, 5.4.4.1: Random Generator
	// produces a random number between 0 and m-1 inclusive
	def rand(X: Int, i: Int, m: Int) = (Constants.V0( (X + i) % 256 ) ^ Constants.V1( ((X/256.).toInt + i) % 256)) % m
}