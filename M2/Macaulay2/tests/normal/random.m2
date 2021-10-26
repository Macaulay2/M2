-- let's make sure the random number generator doesn't change

-- starting in 1.6.0.1 tests (such as this one, as well as examples) are run
-- with the random number seed initialized with "setRandomSeed 0".  Here we
-- confirm that:
assert ( ( for i to 20 list random 100 ) === {24, 65, 71, 72, 19, 19, 91, 72, 93, 79, 72, 77, 63, 85, 39, 21, 34, 19, 54, 62, 83} )
setRandomSeed 0
assert ( ( for i to 20 list random 100 ) === {24, 65, 71, 72, 19, 19, 91, 72, 93, 79, 72, 77, 63, 85, 39, 21, 34, 19, 54, 62, 83} )

setRandomSeed()
-- starting with 1.2.1, we initialize the random number seed "randomly", 
-- and the way to get it the way it was before is with setRandomSeed()

f = result -> assert( apply(20, i -> random 20) === result )
f {11, 15, 3, 8, 1, 3, 9, 11, 15, 15, 14, 7, 16, 3, 9, 2, 6, 3, 5, 16}
-- before 1.1 it was:
-- f {10, 11, 15, 12, 1, 1, 10, 17, 18, 1, 14, 0, 6, 16, 10, 8, 19, 5, 4, 14}

-- let's make sure the random number generator seed setter doesn't change
setRandomSeed 1234567
f {3, 7, 18, 12, 1, 14, 8, 14, 18, 19, 5, 4, 5, 5, 3, 19, 15, 15, 19, 19}
-- f {9, 12, 8, 13, 18, 8, 19, 3, 10, 19, 14, 9, 14, 12, 0, 4, 15, 1, 2, 11}
setRandomSeed "ABCDEFGHIJKLMNOP"
f {1, 8, 9, 0, 4, 3, 16, 15, 4, 0, 4, 16, 18, 12, 3, 11, 11, 19, 12, 18}
-- f {14, 4, 16, 13, 5, 13, 18, 7, 9, 19, 13, 10, 7, 15, 9, 2, 1, 3, 11, 7}

-- let's make sure the seed setter uses all the letters of the string
setRandomSeed "ABCDEFGHIJKLMNOQ"
f {6, 8, 10, 2, 2, 11, 10, 9, 7, 17, 0, 4, 1, 5, 17, 18, 13, 10, 13, 5}
-- f {1, 13, 9, 11, 8, 18, 2, 5, 12, 1, 13, 15, 12, 17, 16, 5, 8, 12, 4, 19}
setRandomSeed "=BCDEFGHIJKLMNOP"
f {15, 0, 12, 9, 9, 6, 13, 3, 0, 1, 3, 15, 8, 5, 2, 16, 17, 7, 5, 3}
-- f {14, 13, 15, 13, 0, 18, 18, 5, 12, 18, 5, 3, 0, 13, 9, 5, 19, 10, 7, 3}

-- let's make sure the seed setter treats the 32nd bit properly
-- one possible bug is to propagate it as the sign bit to a 64 bit int
setRandomSeed (2^31 + 5)
f {10, 19, 5, 12, 3, 9, 15, 18, 10, 12, 0, 4, 9, 14, 9, 16, 10, 12, 7, 15}
-- f {9, 11, 10, 5, 5, 5, 18, 17, 10, 15, 1, 12, 4, 10, 9, 13, 12, 18, 11, 18}
