structure Xoshiro256 :> PRNG =
struct
type intU = Word64.word
type seed = intU * intU * intU * intU

val maxUInt = 0wxFFFFFFFFFFFFFFFF: intU

fun timeToNano () = (Word64.fromLargeInt o Time.toNanoseconds o Time.now) ()

fun splitmix64 state = let
    val (c1, c2, c3) = (0wx9E3779B97f4A7C15: intU,
			0wxBF58476D1CE4E5B9: intU,
			0wx94D049BB133111EB: intU)
    val res1 = state + c1
    val res2 = c2 * Word64.xorb (res1, Word64.>> (res1, 0w30))
    val res3 = c3 * Word64.xorb (res2, Word64.>> (res2, 0w27))
in (Word64.xorb (res3, Word64.>> (res3, 0w31)), res1) end

fun initializeSeed s = let val (seed1, state1) = splitmix64 s
			   val (seed2, state2) = splitmix64 state1
			   val (seed3, state3) = splitmix64 state2
			   val (seed4, _) = splitmix64 state3
		       in (seed1, seed2, seed3, seed4) end

val generateSeed = initializeSeed o timeToNano

val seedFromNumber = initializeSeed

fun rotl (x, k) = Word64.orb (Word64.<< (x, k), Word64.>> (x, 0w64 - k))

(* The aposthrophes are a stand-in for the stars [xoshiro256**]. *)
fun xoshiro256'' (a, b, c, d) = let val result = rotl (b * 0w5, 0w7) * 0w9
				    val t = Word64.<< (b, 0w17)
				    val c = Word64.xorb (c, a)
				    val d = Word64.xorb (d, b)
				    val b = Word64.xorb (b, c)
				    val a = Word64.xorb (a, d)
				    val c = Word64.xorb (c, t)
				    val d = rotl (d, 0w45)
				in (result, (a, b, c, d)) end

val randomNumber = xoshiro256''

(* Return a random number from 0 to n-1 based on seed. *)
fun randomToN (n, seed) = let val (rand, st) = xoshiro256'' seed
			  in (rand div (maxUInt div n), st) end
end
