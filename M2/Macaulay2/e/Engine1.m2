needs "Engine.m2"

-- This file contains basic tests about creating ZZ,Z/p,simple poly ring,
-- and ring element operations.

-- Test of trivial rings
R1 = ZmodP 5
assert(see R1 == "ZZ/5[EMonomialOrder[]]")
R2 = ZmodP 32003
assert(see R2 == "ZZ/32003[EMonomialOrder[]]")
assert(see EZ == "ZZ[EMonomialOrder[]]")
assert(see (ZZ/5) == "ZZ/5")
assert(see ZZ == "ZZ")

-- Test of trivial monoid
motrivial = monomialOrder()
M = emonoid(motrivial,{},"")
R = polyring(ZmodP 5, M, EZ, {})
R1 = R^1
one = R1_({},0)
assert(4*one == -one)
assert(5*one == 0)

-- Test of ring operations in EZ
assert(see EZ == "ZZ[EMonomialOrder[]]")
assert(see (1_EZ) == "1")
assert(1_EZ + 3_EZ == 4_EZ)
assert(0_EZ + 100_EZ == 100_EZ)
assert(1_EZ * 1234_EZ == 1234_EZ)
assert(0_EZ * 1234_EZ == 0_EZ)
assert(1_EZ - 1_EZ == 0_EZ)
assert(size(1_EZ - 1_EZ) == 0)
assert(- 1_EZ == (-1)_EZ)
assert(1_EZ != 0_EZ)
(2_EZ)^31  -- FAILS since infinite precision is not ready yet
assert(degree (1_EZ) == {})  -- no degrees in this ring

-- Test of ring operations in ZZ/p
R = ZmodP 101
assert(see R == "ZZ/101[EMonomialOrder[]]")
assert(see (1_R) == "1")
assert(1_R + 3_R == 4_R)
assert(0_R + 100_R == 100_R)
assert(1_R * 1234_R == 1234_R)
assert(0_R * 1234_R == 0_R)
assert(1_R - 1_R == 0_R)
assert(size(1_R - 1_R) == 0)
assert(- 1_R == (-1)_R)
assert(1_R != 0_R)
assert((2_R)^31 == 34_R)
assert(degree (1_R) == {})  -- no degrees in this ring
assert(101_R == 0_R)
assert(100_R == (-1)_R)

----------------------------------------
-- Simple commutative polynomial ring --
-- Ring element arithemtic            --
----------------------------------------
M = emonoid(monomialOrder 4, toList(0..3), "a b c d")
K = ZmodP 101
R = polyring(K, M, degreeRing 1, {1,1,1,1})
assert(size (1_R) == 1)
assert(size(0_R) == 0)
assert(size(101_R) == 0)
a = R_(1_K,{0,1})
b = R_(1_K,{1,1})
c = R_(1_K,{2,1})
d = R_(1_K,{3,1})
f = R_(2_K,{0,3,1,2})
g = R_(-1_K,{1,1,2,0,1,3,3,4})
assert(R_(0,44) == a^44)
h = f+g+1_R  -- BUG: display
assert(f == 2*a^3*b^2)
assert(g == -b^4*d^4)
assert(a+b == b+a)
assert(size(a-b) == 2)
assert(size(a+b-a) == 1)
assert(size(a+b-a-b) == 0)
a-1_R  -- BUG: display is bad.
assert(-f == (-1)_R * f)
assert(3*h == h+h+h)
assert(h^4 == (h^2)*(h^2))
try (
  h_(a^3*b^2)
) else (print "coefficient of a monomial not yet implemented"; true)  -- NOT DONE: coefficients 
assert(leadTerm h == f)
assert(leadCoefficient h == 2)  -- PROBLEM: this should come as an element of EZ?
assert(leadComponent h == 0)
assert(leadMonomial h == {0,3,1,2})
assert(getTerms(h,0,0) == leadTerm h)
assert(h == leadTerm h + getTerms(h,1,-1))
-- degrees
assert(degree h == {8})
assert(degreeWeights(h,{1,1,1,1}) == {0,8})
h1 = homogenize(h,0,{1,1,1,1})
assert(degreeWeights(h1,{1,1,1,1}) == {8,8})
assert(isGraded h1)
assert(not isGraded h)
-- ggtonet
assert(ascii callgg(ggtonet, a) == flatten{1, 0,{1,0,1},1})
h = a+b
assert(ascii callgg(ggtonet, h) == flatten flatten{2, {0,{1,0,1},1}, {0,{1,1,1},1}})
assert(ascii callgg(ggtonet, 0_R) == {0})
assert(ascii callgg(ggtonet, 2_R) == flatten flatten {1, {0,{0},2}})
h = a*b^2-c*d^5*a+1_R  -- BUG ??
assert(ascii callgg(ggtonet, h) == flatten flatten {3, {0,{2,0,1,1,2},1},  
	                                           {0,{3,0,1,2,1,3,5},128,100}, 
						   {0,{0},1}})
h = c*d + 1_R -- BUG: sometimes is coming out to be 2*c*d??
-- NOT DONE: division, remainder, gcd, fast power routine,
-- coefficientOf, exponents, monomials.
-- diff, contract
