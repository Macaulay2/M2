needs "Engine.m2"
----------------------------------
-- Test of freemodule routines ---
----------------------------------
-- Test A: ring with trivial degree monoid.
M = emonoid(monomialOrder 4, toList(0..3), "a b c d")
R = polyring(ZmodP 101, M, EZ, {})
R1 = R^1
R1a = R^1
assert(R1 == R1a)
try (
  assert(handle R1 === handle R1a);  -- fails
) else ( print "handle equivalence fails"; true)

assert(R^3 == R^3)
assert(not(R^3 == R^4))
assert(try (R^(-1)) else true)
F = R^0
assert(rank F == 0)
assert(degrees F === {})
assert(F == dual F)
assert(F ++ F == F)
assert(F ** F == F)
F = R^3 ++ R^7
assert(rank F === 10)
assert(degrees F == {})
F = R^3 ** R^7
assert(F == R^21)
assert(rank F == 21)
assert(degrees F === {})
F = dual R^53
assert(F == R^53)
assert(rank F == 53)
assert(degrees F === {})
F = submod(R^19,{0,1,6})
assert(F == R^3)
assert(rank F == 3)
assert(degrees F === {})
F = exterior(3,R^6)
assert(F == R^20)
F = exterior(6,R^6)
assert(F == R^1)
F = exterior(7,R^6)
assert(F == R^0)
F = exterior(1,R^6)
assert(F == R^6)
F = exterior(0,R^6)
assert(F == R^1)
F = exterior(-1,R^6)
assert(F == R^0)

-- Test B: with a single degree
M = emonoid(monomialOrder 4, toList(0..3), "a b c d")
K = ZmodP 101
R = polyring(K, M, degreeRing 1, {1,1,1,1})

F = R^{1,2,3}
G = R^{10,20,-30,40}
assert(F == R^{1,2,3})
assert(degrees dual F == {-1,-2,-3})
assert(degrees(F ++ F) == {1,2,3,1,2,3})
assert(degrees(F ** F) == {2,3,4,3,4,5,4,5,6})
assert(degrees submod(F,{0,2,2,0}) == {1,3,3,1})
assert(degrees exterior(2,F) == {3,4,5})
-- MISSING: symm(p,F)
assert(degrees(F ++ G) == join(degrees F, degrees G))
assert(degrees(F ** G) == flatten apply(degrees F, d -> apply(degrees G, e -> d+e)))


-- Test C: with a bi-grading
M = emonoid(monomialOrder 4, toList(0..3), "a b c d")
K = ZmodP 101
R = polyring(K, M, degreeRing 2, flatten{{-1,0,0,0},{0,1,0,0}})  -- DISPLAY needs degree info.
a = R_(1_K,{0,1})
b = R_(1_K,{1,1})
c = R_(1_K,{2,1})
d = R_(1_K,{3,1})
assert(degree a == {-1,0})
assert(degree b == {0,1})
assert(degree c == {0,0})
assert(degree d == {0,0})

degs = {1,0,0,1,5,6,10,10} 
F = R^degs
assert(rank F == 4)     
assert(degrees F == degs)
assert(degrees dual F == - degs)
assert(degrees (F ++ F) == join(degs,degs))
assert(degrees submod(F,{0,2,1}) == {1, 0, 5, 6, 0, 1})
assert(degrees exterior(2,F) == {1,1,  6,6,  5,7,  11,10,  10,11,  15,16})

assert(degree (F_0) == {1,0})
assert(degree (F_1) == {0,1})
assert(degree (F_2) == {5,6})
assert(degree (F_3) == {10,10})
assert(try F_4 else true)
assert(try F_(-1) else true)

-- Let's make sure that we can check for a Schreyer order in this case:
m = inducedOrder F
assert(src m == F)
assert(targ m == R^0)

-- Test D: with a Schreyer order
M = emonoid(monomialOrder 4, toList(0..3), "a b c d")
K = ZmodP 101
R = polyring(K, M, degreeRing 1, {1,1,1,1})
a = R_(1_K,{0,1})
b = R_(1_K,{1,1})
c = R_(1_K,{2,1})
d = R_(1_K,{3,1})

m = ematrix(R,{{a*b,c*d,a^2}})
G = src m
assert(degrees G == {2,2,2})
F = R^m
assert(rank F == 3)
assert(degrees F  == {2,2,2})
assert(inducedOrder F - m == 0)

n = ematrix(R, {{a*b,0_R},{0_R,b^2}})
G = R^n
H = F ** G
inducedOrder H  -- Is this what we really want?
H = F ++ G
inducedOrder H  -- Not correct?
assert(rank targ inducedOrder dual F == 0)
-- Test of arithmetic in this order NEEDS TO BE DONE

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/e"
-- End:
