-- Test of symmetric powers of schreyer orders
-- and of free modules

R = QQ[a,b,c, Degrees => {{1,0,0},{0,1,0},{0,1,3}}]
F = source vars R
assert(degrees source symmetricPower(2,vars R) 
     == {{2, 0, 0}, {1, 1, 0}, {1, 1, 3}, {0, 2, 0}, {0, 2, 3}, {0, 2, 6}})
assert(degrees source symmetricPower(1, matrix{{a^2,a*b,b^3}}) == {{2, 0, 0}, {1, 1, 0}, {0, 3, 0}})
assert(degrees source symmetricPower(0, matrix{{a^2,a*b,b^3}}) == {{0, 0, 0}})
assert(degrees source symmetricPower(-1, matrix{{a^2,a*b,b^3}}) == {})
assert(degrees source symmetricPower(2, matrix{{a^2,a*b,b^3}}) ==
     {{4, 0, 0}, {3, 1, 0}, {2, 3, 0}, {2, 2, 0}, {1, 4, 0}, {0, 6, 0}})
assert(degrees source symmetricPower(3, matrix{{a^2,a*b,b^3}}) ==
     {{6, 0, 0}, {5, 1, 0}, {4, 3, 0}, {4, 2, 0}, {3, 4, 0}, 
      {2, 6, 0}, {3, 3, 0}, {2, 5, 0}, {1, 7, 0}, {0, 9, 0}})
     


R = QQ[a..d]
f = schreyerOrder vars R
debug Core
raw f
rawTarget raw f
rawSource raw f

g = symmetricPower(3,f)
rawSource raw g
assert(schreyerOrder source g == diagonalMatrix g)

g = symmetricPower(3,f)
rawSource raw g
assert(schreyerOrder source g == diagonalMatrix g)

f = schreyerOrder matrix{{a^3-b*c, a*b*c-c-1, 2*c*d-1}}
g = symmetricPower(3,f)
rawSource raw g
assert(schreyerOrder source g == diagonalMatrix matrix{(flatten entries g)/leadMonomial})


----- test of schreyer orders with exterior power
R = QQ[a..d]
f = schreyerOrder vars R
debug Core
raw f
F = source f
raw F
F2 = exteriorPower(2,F) 
F2' = source schreyerOrder schreyerOrder F2
raw F2'
raw F2
F4 = exteriorPower(4,F) 
schreyerOrder F4
assert(exteriorPower(5,F) == 0)
