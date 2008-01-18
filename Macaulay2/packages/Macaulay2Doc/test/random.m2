-- with 1.1 we no longer try to keep the random number generator from changing

end

-- let's make sure the random number generator doesn't change
f = result -> assert( apply(20, i -> random 20) === result )
f {10, 11, 15, 12, 1, 1, 10, 17, 18, 1, 14, 0, 6, 16, 10, 8, 19, 5, 4, 14}

-- let's make sure the random number generator seed setter doesn't change
setRandomSeed 1234567
f {9, 12, 8, 13, 18, 8, 19, 3, 10, 19, 14, 9, 14, 12, 0, 4, 15, 1, 2, 11}
setRandomSeed "ABCDEFGHIJKLMNOP"
f {14, 4, 16, 13, 5, 13, 18, 7, 9, 19, 13, 10, 7, 15, 9, 2, 1, 3, 11, 7}

-- let's make sure the seed setter uses all the letters of the string
setRandomSeed "ABCDEFGHIJKLMNOQ"
f {1, 13, 9, 11, 8, 18, 2, 5, 12, 1, 13, 15, 12, 17, 16, 5, 8, 12, 4, 19}
setRandomSeed "=BCDEFGHIJKLMNOP"
f {14, 13, 15, 13, 0, 18, 18, 5, 12, 18, 5, 3, 0, 13, 9, 5, 19, 10, 7, 3}

-- let's make sure the seed setter treats the 32nd bit properly
-- one possible bug is to propagate it as the sign bit to a 64 bit int
setRandomSeed (2^31 + 5)
f {9, 11, 10, 5, 5, 5, 18, 17, 10, 15, 1, 12, 4, 10, 9, 13, 12, 18, 11, 18}
