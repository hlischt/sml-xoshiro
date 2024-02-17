signature PRNG =
sig
    type intU = Word64.word
    type seed
    val maxUInt: intU
    val generateSeed: unit -> seed
    val seedFromNumber: intU -> seed
    val randomNumber: seed -> intU * seed
    val randomToN: intU * seed -> intU * seed
end
