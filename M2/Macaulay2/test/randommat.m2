-- let's make sure the random number generator doesn't change

assert( random (ZZ^3, ZZ^3) == matrix {{0, 2, 0}, {1, -9, 7}, {5, -9, 8}} )
