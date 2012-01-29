package Raptor

class Encoder {
	// values as per recommendation in RFC 5053, 4.2
	val Kmin = 1024.// a minimum target on the number of symbols per source block
	val Gmax = 10.	// a maximum target number of symbols per packet
	val Al = 4 		// a symbol alignment parameter, in bytes

	val W = 100		// a target on the sub-block size, in bytes

	def encode(data: List[Byte], payloadSize: Int) {
		// the transfer length of the object, in bytes
		val F = data.length	
			
		// maximum packet payload size, in bytes, which
		// is assumed to be a multiple of Al
		val P = payloadSize
		
		// == RFC 5053, 4.2: Example Parameter Derivation Algorithm 
		// maximum number of symbols to be transported in a single packet
		val G = math.min(math.ceil(P*Kmin/F), P/Al, Gmax)

		// symbol size
		val T = math.floor(P/(Al*G))*Al

		// total number of symbols required to represent the source data of the object
		val Kt = math.ceil(F/T)

		// number of source blocks
		val Z = math.ceil(Kt/Kmax)

		// number of sub-blocks in each source block,
		val N = math.min(math.ceil(math.ceil(Kt/Z)*T/W), T/Al)

		// == 5.3.1.2: Source Block and Sub-Block Partitioning
		val (kl, ks, zl, zs) = Tools.Partition(Kt, Z)
		val (tl, ts, nl, ns) = Tools.Partition(T/Al, N)	 				
	}


}