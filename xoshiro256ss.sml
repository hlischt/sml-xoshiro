(* xoshiro256starstar / xoshiro256** *)

structure x256ss : XOSHIRO =
struct
type intU = Word64.word
type seed = intU * intU * intU * intU

fun rotl (x, k) = Word64.orb (Word64.<< (x, k), Word64.>> (x, 0w64 - k))

fun xoshiro (a, b, c, d) = let val result = rotl (b * 0w5, 0w7) * 0w9
			       val t = Word64.<< (b, 0w17)
			       val c = Word64.xorb (c, a)
			       val d = Word64.xorb (d, b)
			       val b = Word64.xorb (b, c)
			       val a = Word64.xorb (a, d)
			       val c = Word64.xorb (c, t)
			       val d = rotl (d, 0w45)
			   in (result, (a, b, c, d)) end
end
