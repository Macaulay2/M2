-- floorLog test#1 
TEST ///
time J = floorLog(2,3);
assert(J == 1)
///

TEST ///
time J = floorLog(5,26);
assert(J==2)
///

TEST ///
time J = floorLog(5,2);
assert(J==0)
///


-- multiplicativeOrder test#1 
TEST ///
time J = multiplicativeOrder(10, 7);
assert(J == 6)
///

TEST ///
time J = multiplicativeOrder(1, 1);
assert(J == 1)
///

TEST ///
time J = multiplicativeOrder(408, 409);
assert(J == 2)
///

-- divideFraction test#1 - (denominator a power of p)
TEST ///
time J = decomposeFraction(7, 19/49);
assert(J == (19, 2, 0) )
///

-- divideFraction test#2 - (denominator not power of p)
TEST ///
time J = decomposeFraction(2, 5/56);
assert(J == (5, 3, 3) )
///

-- divideFraction test#3 - (denominator not power of p)
TEST ///
time J = decomposeFraction(2, 5/24);
assert(J == (5, 3, 2) )
///

-- divideFraction test#4 - (negative)
TEST ///
time J = decomposeFraction(7, -19/49);
assert(J == (-19, 2, 0) )
///

-- adicDigit tests
TEST ///
time J = adicDigit(7, 2, 0);
assert(J == 0)
///

TEST ///
time J = adicDigit(13, 100, 1);
assert(J == 12)
///

TEST ///
time J = adicDigit(3, 2, 3/4);
assert(J == 0)
///

TEST ///
time J = adicDigit(3, 1, 3/4);
assert(J == 2)
///

TEST ///
time J = adicDigit(5, 3, 1/13);
assert(J == 4)
///

TEST ///
L = {3/4, 1/13};
time J = adicDigit(5, 3, L);
assert(J == {3,4})
///

-- adicExpansion tests 
TEST ///
time J = adicExpansion(2, 22);
assert(J == {0, 1, 1, 0, 1})
///

TEST ///
time J = adicExpansion(5, 399);
assert(J == {4, 4, 0, 3})
///

TEST ///
time J = adicExpansion(2, 4, 1/5);
assert(J == {0, 0, 1, 1})
///

TEST ///
time J = adicExpansion(7, 7, 1/19);
assert(J == {0, 2, 4, 0, 2, 4,0})
///

-- adicTruncation
TEST ///
time J = adicTruncation(7, 4, 1/19);
assert(J == 18/343)
///

TEST ///
time J = adicTruncation(7, 4, 1/29);
assert(J == 82/2401)
///

TEST ///
time J = adicTruncation(7, 4, {1/19, 1/29});
assert(J == {18/343, 82/2401})
///







