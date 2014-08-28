-- Test of the tower ring engine functions
{*
restart
*}

debug Core

-- example 1
R1 = rawTowerRing(7, (1:"a"))
assert(rawCharacteristic R1 == 7)
a = R1_0
F = a^35+2*a^14+4*a^7+1
assert(rawLowerP F == a^5 + 2*a^2 + 4*a + 1)
assert(rawDiff(0,F) == 0)

-- example 2
R1 = rawTowerRing(2, ("a","y"))
a = R1_0
y = R1_1
R2 = rawTowerQuotientRing(R1, (a^3+a^2+1, 0_R1))
assert(rawCharacteristic R2 == 2)
rawExtensionDegree(0,R2) == -1 -- why!?
rawExtensionDegree(1,R2) == 3
assert(rawExtensionDegree(0,R2) == -1)
a = R2_0
y = R2_1
F = y^4 + a * y^2 + (a+1)
assert(rawDegree(1,rawLowerP F) <= 2)
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
assert(rawDiff(0, y^3 + a*y^5 + 1) == y^2 + a*y^4)

-- 
debug Core
R1 = rawTowerRing(2, 1:"x")
x = R1_0
F = poly"x197+x196+x195+x192+x189+x187+x180+x179+x178+x175+x174+x171+x168+x167+x165+x164+x162+x161+x158+x157+x156+x154+x150+x147+x143+x141+x137+x136+x135+x134+x132+x131+x130+x129+x127+x126+x125+x123+x122+x120+x117+x116+x115+x113+x112+x111+x108+x102+x100+x99+x98+x97+x95+x94+x93+x92+x90+x89+x87+x86+x84+x81+x77+x74+x73+x69+x66+x65+x62+x61+x59+x58+x57+x56+x54+x53+x51+x49+x47+x46+x45+x44+x43+x40+x39+x38+x36+x34+x33+x31+x25+x23+x21+x20+x18+x17+x16+x15+x12+x9+x2+x+1"
--distinctDegreeFactorization F -- this function is in TowerRings...
x2 = rawPowerMod(x,2,F)
x3 = rawPowerMod(x2,2,F)
x4 = rawPowerMod(x3,2,F)
x5 = rawPowerMod(x4,2,F)
x6 = rawPowerMod(x5,2,F)
rawGCD(x2-x,F)
rawGCD(x3-x,F)
rawGCD(x4-x,F)
rawGCD(x5-x,F)

