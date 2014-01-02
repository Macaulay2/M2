-- Test of raw tower ring routines
-- Will be changed once these are more top level operations

----------------------------
-- Display of polynomials --
----------------------------
-- Test 1. Printing.  Need to make sure that what we see is correct!
--  Test 1A. 1 variable.
debug Core
R = rawTowerRing(101, 1:"a")
a = rawRingVar(R,0)
assert(toString(a) == "a")
assert(toString(a*a) == "a2")
assert(toString(a^2) == "a2")
assert(toString(7*a^2) == "7a2")
assert(toString(1+a) == "a+1")
assert(toString(a-a) == "0")
assert(toString(1+a+a^2) == "a2+a+1")
assert(toString(3+a^2) == "a2+3")
assert(toString(2*a^2+1) == "2a2+1")
toString(3*a^10+4*a^3+12) == "3a10+4a3+12"
--  Test 1B. 2 variable.
debug Core
R = rawTowerRing(101, ("a", "b"))
a = rawRingVar(R,0)
b = rawRingVar(R,1)
assert(toString(a+b) == "b+a")
assert(toString(3*a^2+2*b) == "2b+3a2")
assert(toString((a+1)*b^3+1) == "(a+1)b3+1")
assert(toString((a+1)*b^3+(2*a+1)*b+(a+2)) == "(a+1)b3+(2a+1)b+a+2")

--  Test 1B. 3 variable.
debug Core
R = rawTowerRing(101, ("a", "b", "c"))
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
assert(toString a == "a")
assert(toString b == "b")
assert(toString c == "c")
assert(toString(a+b) == "b+a")
assert(toString(b^3+c^3) == "c3+b3")
assert(toString(2*b+c) == "c+2b")
assert(toString((a+b^4)*c^10+(a+1)*c^5+13*c^2+12*b) == "(b4+a)c10+(a+1)c5+13c2+12b")
assert(toString((a+b^4)*c^10+(a+1)*c^5+13*c^2+(a+1)*b+a^3+a) == "(b4+a)c10+(a+1)c5+13c2+(a+1)b+a3+a")
assert(0 == rawRingVar(R,3))
assert(toString(1_R) == "1")
assert(toString(100_R) == "100")
assert(toString(0_R) == "0")
assert(toString(101_R) == "0")
assert(101_R == 0)

---------------------------------
-- Arithmetic operations --------
---------------------------------
-- operations:
--  n_R (n integer or rational)
--  promote and lift (not yet written)
--  add, subtract, negate, mult, divide (exact division)
--  random
--  syzygy
debug Core
R = rawTowerRing(17, 1:"a")
a = rawRingVar(R,0)
toString(a) == "a"
F = a^2+a+1
F1 = 2*F
F2 = 15*F
assert(F1+F2 == 0)
assert((2*F) + (15*F) == 0)

debug Core
R = rawTowerRing(17, ("a", "b", "c"))
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
assert((15*a) + (2*a) == 0)
assert((15*b) + (2*b) == 0)
assert((15*c) + (2*c) == 0)
assert(15_R + 2_R == 0)
assert((15*a) + (2*a+1) == 1_R)
assert((15*a - a^4) + (2*a+a^4) == 0)
F = 15*a
assert(F + (-F) == 0)
assert(F - F == 0)

------------------------------------
-- Multiplication and powers -------
------------------------------------
-- NOTE: 5/20/10: mult is much slower than in poly rings, at least
--  for dense polynomials?
debug Core
R = rawTowerRing(17, ("a", "b", "c"))
a = rawRingVar(R,0)
b = rawRingVar(R,1)
c = rawRingVar(R,2)
F = a+b
G = a+c
F*G == G*F
a*a + a*c + a*b + b*c == F*G
F^3
F = a+b+c
{*
time F = F^50;
time(F*F);
*}

--------------------------------------
-- Quotient rings --------------------
--------------------------------------
debug Core
R = rawTowerRing(17, ("a","b"))
a = rawRingVar(R,0)
b = rawRingVar(R,1)
F = a^3+a+1
G = b^2+a
A = rawTowerQuotientRing(R, (F, G))
a = rawRingVar(A,0)
b = rawRingVar(A,1)
a^2
a
a*a
16*a^3
(b+1)*(b+1)
b*b
assert(b^2 == -a)

debug Core
R = rawTowerRing(17, 1:"a")
a = rawRingVar(R,0)
F = a^3+a+1
A = rawTowerQuotientRing(R, 1:F)
a = rawRingVar(A,0)
a^2
assert(a^3 == -a-1)
assert(a * rawInverse a == 1_A)

debug Core
R = rawTowerRing(17, 1:"a")
a = rawRingVar(R,0)
F = a^2+2*a+1
A = rawTowerQuotientRing(R, 1:F)
a = rawRingVar(A,0)
{*
rawInverse (a+1) -- error not considered?
*}

a^2 
a^3
assert(a * rawInverse a == 1_A)

--------------------------------------
-- gcd -------------------------------
--------------------------------------
debug Core
testGCD = (F1,F2) -> (
     G1 := rawGCD(F1,F2);
     (G,U,V) := rawExtendedGCD(F1,F2);
     assert(G == U*F1 + V*F2);
     assert(G == G1);
     (G,U,V)
     )

R = rawTowerRing(17, 1:"a")
a = rawRingVar(R,0)
F1 = (a+1)*(a+2)
F2 = (a+1)*(2*a+3)
(G,U,V) = testGCD(F1,F2)

assert(rawGCD(0_R,F1) == F1)
assert(rawGCD(F1,0_R) == F1)
assert(rawGCD(0_R,F2) == 9*F2) -- good: this is the monic-ification of F2
assert(rawGCD(F2,0_R) == rawGCD(0_R,F2))
assert(rawGCD(0_R,0_R) == 0)

F1 = (a+1)*(a+2)^10
F2 = (a+1)^10*(2*a+3)
(G,U,V) = testGCD(F1,F2)

F1 = (2*a^3+a+1)*(a+2)^10
F2 = (2*a^3+a+1)^10*(2*a+3)
(G,U,V) = testGCD(F1,F2)

-- example: gcd over a quotient ring
debug Core
R = rawTowerRing(17, ("a","b"))
a = rawRingVar(R,0)
b = rawRingVar(R,1)
F = a^3+a+1
A = rawTowerQuotientRing(R, (F, 0_R))
a = rawRingVar(A,0)
b = rawRingVar(A,1)
b^2-a-1

F1 = a*(b-a-1)^3*(b^2-a-1)
F2 = (a^2+a+1)*(b-a-1)*(b^2-a-2)^3
(G,U,V) = testGCD(F1,F2)
assert(rawGCD(0_A,F1) == (b-a-1)^3*(b^2-a-1))
assert(rawGCD(F1,0_A) == (b-a-1)^3*(b^2-a-1))

testGCD(0_A,F1)
testGCD(F1,0_A)
rawGCD(F1,0_A)
(G,U,V) = rawExtendedGCD(F1,0_A)
assert(U * F1 + V * 0_A == G)

U 
F1 = (b-a)*a
F2 = (b-a)^2*b
(G,U,V) = testGCD(F1,F2) -- ok

F1 = b*(b+1)
F2 = b^2*(b+2)
(G,U,V) = testGCD(F1,F2) -- ok

F1 = (b-a)*(b+a)
F2 = (b-a)^2*b
(G,U,V) = testGCD(F1,F2) -- ok

F1 = (b^4-a*b-1)^100*(b-a-1);
F2 = (b^4-a*b-1)*(b-a-1)^100;
time (G,U,V) = testGCD(F1,F2) -- G is 0
assert(G == 0) -- the monic gcd alg can't find a monic gcd

-- example: gcd over a quotient ring which is a domain
debug Core
R = rawTowerRing(17, ("a","b"))
a = rawRingVar(R,0)
b = rawRingVar(R,1)
F = a^3-a-2
A = rawTowerQuotientRing(R, (F, 0_R))
a = rawRingVar(A,0)
b = rawRingVar(A,1)

time F1 = (b^4-a*b-1)^100*(b-a-1);
F2 = (b^4-a*b-1)*(b-a-1)^100;
time (G,U,V) = rawExtendedGCD(F1,F2)
time (G,U,V) = testGCD(F1,F2)
F12 = (b^4-a*b-1)*(b-a-1)
assert(G == F12)

end
load "~/src/M2/Macaulay2/packages/Macaulay2Doc/test/tower.m2"

S = ZZ/17[x,y,z]
G = (x+y+z)^50;
time(G*G);

S = ZZ/17[symbol a]
F1 = (2*a^3+a+1)*(a+2)^10
F2 = (2*a^3+a+1)^10*(2*a+3)
gcdCoefficients(F1,F2)
