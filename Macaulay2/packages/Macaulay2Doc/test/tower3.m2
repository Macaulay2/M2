-- Test of some of the tower ring cases

restart
debug Core
loadPackage "TowerRings"

-- example 1
R1 = rawTowerRing(7, (1:"a"))
assert(rawExtensionDegree(-1,R1) == 7)
a = R1_0
F = a^35+2*a^14+4*a^7+1
assert(rawLowerP F == a^5 + 2*a^2 + 4*a + 1)
assert(rawDiff(0,F) == 0)

-- example 2
R1 = rawTowerRing(2, ("a","y"))
a = R1_0
y = R1_1
R2 = rawTowerQuotientRing(R1, (a^3+a^2+1, 0_R1))
a = R2_0
y = R2_1
F = y^4 + a * y^2 + (a+1)
assert(rawDegree(1,rawLowerP F) <= 2) -- fails
assert((rawLowerP F)^2 == F)
rawLowerP F
(a^2+a+1)^2
(a^2+a)^2

-- test of rawDiff
R1 = rawTowerRing(7, (1:"a"))
a = R1_0
F = a^34+2*a^14+4*a^7+1
rawDiff(0,F)
rawDiff(-1,F)
assert(rawDiff(0,a^3+3*a^2+1) == 3*a^2 + 6*a)
assert(rawDiff(0,a^35+2*a^14+4*a^7+1) == 0)
-- test of rawDiff, in more variables
R1 = rawTowerRing(2, ("a","y"))
a = R1_0
y = R1_1
R2 = rawTowerQuotientRing(R1, (a^3+a^2+1, 0_R1))
a = R2_0
y = R2_1
F = y^4 + a * y^2 + (a+1)
assert(rawDiff(0,F) == 0)
rawDiff(0, y^3 + a*y^5 + 1)

-- example of squarefree fact
R1 = rawTowerRing(101, (1:"x"))
x = R1_0
F = (x+1)^3*(x^2+x+1)*x^3*(x+2)^2
assert(squareFreeDecomposition F == {{1, x^2+x+1}, {2, x+2}, {3, x^2+x}})

R1 = rawTowerRing(101, ("a","x"))
a = R1_0
x = R1_1
R2 = rawTowerQuotientRing(R1, (a^3+a^2+1, 0_R1))
a = R2_0
x = R2_1
F = (x+1)^3*(x^2+x+1)*x^3*(x+2)^2
squareFreeDecomposition F 
assert(squareFreeDecomposition F == {{1, x^2+x+1}, {2, x+2}, {3, x^2+x}})

F = (x+a)^3*(x^2+x+a)*x^3*(x+2+a)^2
squareFreeDecomposition F 
