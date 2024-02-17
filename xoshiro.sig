signature XOSHIRO = sig
    type intU = Word64.word
    type seed = intU * intU * intU * intU
    val xoshiro: seed -> intU * seed
end
