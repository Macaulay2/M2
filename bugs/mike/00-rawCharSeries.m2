-- Bugs involving rawCharSeries (16 Jan 2013)
-- These are taken from 
--    00-kahle-isPrime.m2  (crashing bug)
--    test/decompose4.m2 (crashing bug)
--    00-bug-radical (gives WRONG answer)

-----------------
-- example from: 00-kahle-isPrime.m2
-----------------
kk = ZZ/32003 -- this one just goes off into never never land, doesn't return
kk = QQ -- this one crashes very early
R = kk[x1,x2,x3,x4,x5,x6, MonomailOrder=>Lex] -- crashes with this ring and QQ
R = kk[x1,x2,x3,x4,x5,x6, MonomailOrder]  -- crashes with this ring and QQ
I = ideal(x1^2*x2^2*x5^8*x6^8-x3^4*x4^4,x2^8*x3^2*x4^8*x5^2-x1^4*x6^4,x1^8*x3^8*x4^2*x6^2-x2^4*x5^4)
debug Core
m = gens I
rawCharSeries raw m

-----------------
-- example from: test/decompose4.m2
-----------------
kk = QQ
S = kk[a,b,c,d, MonomialOrder=>Lex]
J = ideal(4*c^2+a*d-4*b*d-4*c*d+3*d^2-48*c+16*d+144,
    2*a*c-4*c^2+3*b*d+4*c*d-3*d^2-12*a-4*b+48*c-16*d-144,
    -b*c^2+(1/4)*b^2*d+(1/2)*b*c*d+c^2*d-(1/2)*b*d^2-(1/2)*c*d^2+(1/4)*d^3+10*b*c-4*b*d-10*c*d-24*b+28*d)
J1 = ideal(J_0, J_1, 4*J_2)
  -- the only difference between J and J1 is the factor of 4 on the last generator of J
debug Core
rawCharSeries raw gens J1 -- works
rawCharSeries raw gens J -- CRASHES not so much

-----------------
-- example from: 00-bug-radical
-----------------
-- new attempt to boil it down:
-- Dan, I think that rawCharSeries is giving an incorrect answer here.
-- Could you run this on your machine and see if you have a problem?
restart
errorDepth=0
debug Core
R = QQ[x2, x7, x6, x1, x4, x3, x5, MonomialOrder=>Lex]
I = ideal(x4*x3-x4-x3+1,
    x2*x1+x2+x1+1,
    x7*x6*x3-x7*x6-x7*x3+x7+x6*x3-x6-x3+1,
    x2*x7*x3+x2*x7+x2*x3+x2-x7*x3-x7-x3-1,
    x2*x6*x5-x2*x6-x2*x5+x2+x6*x1*x5-x6*x1-x1*x5+x1,
    x2*x7*x5+x2*x7+x2*x3*x5+x2*x3+2*x7*x1-x7*x5+x7-x1*x4*x5+x1*x4+2*x1*x3-x1*x5+x1-x4*x5+x4-x3*x5+x3-x5+1,
    x1*x4*x5-x1*x4+x1*x3*x5-x1*x3+x4*x5-x4+x3*x5-x3,
    x7*x6*x4*x5+x7*x6*x4-x7*x6*x5-x7*x6-x7*x4*x5-x7*x4+x7*x5+x7+x6*x4*x5+x6*x4-x6*x5-x6-x4*x5-x4+x5+1,
    x7*x1*x4*x5-x7*x1*x4+x7*x1*x5-x7*x1+x7*x4*x5-x7*x4+x7*x5-x7+x1*x4*x5-x1*x4+x1*x5-x1+x4*x5-x4+x5-1,
    x6*x1*x4*x5-x6*x1*x4+x6*x1*x5-x6*x1+x6*x4*x5-x6*x4+x6*x5-x6-x1*x4*x5+x1*x4-x1*x5+x1-x4*x5+x4-x5+1,
    x2*x6*x3*x5-x2*x6*x3-x2*x6*x5+x2*x6-x2*x3*x5+x2*x3+x2*x5-x2-x6*x3*x5+x6*x3+x6*x5-x6+x3*x5-x3-x5+1)
rawIdealReorder raw gens I  -- identity, so no reordering is being done
C = rawCharSeries raw gens I -- this is WRONG (seems to me to be wrong, at least)
  -- answer is (1/16/2013)
{*
COD4  x4-1 x1+1 x7+1 x2x6x3x5-x2x6x3-x2x6x5+x2x6-x2x3x5+x2x3+x2x5-x2-x6x3x5+x6x3+x6x5-x6+x3x5-x3-x5+1 
COD4   x4-1 x1+1 x6-1 x2x7x3+x2x7+x2x3+x2-x7x3-x7-x3-1 
  x4-1 x1+1 x6-1 x7+1 x2x3x5+x2x3-x2x5-x2-x3x5-x3+x5+1 
COD4  x3-1 x1+1 x7x6x4x5+x7x6x4-x7x6x5-x7x6-x7x4x5-x7x4+x7x5+x7+x6x4x5+x6x4-x6x5-x6-x4x5-x4+x5+1 x2x6x5-x2x6-x2x5+x2-x6x5+x6+x5-1 
COD4  x3-1 x1+1 x6-1 x7+1 
  x3-1 x4+1 x6-1 x7+1 x2+1 
  x5+1 x4-1 x1+1 x6-1 x7+1 
  x5+1 x3-1 x1+1 x7+1 x2x6-x2-x6+1 
  x5+1 x3+1 x4-1 x6-1 x7+1 x2+1 
  x5+1 x3+1 x4-1 x1+1 x6-1 
  x3-1 x4-1 x1+1 x7+1 x2x6x5-x2x6-x2x5+x2-x6x5+x6+x5-1 
COD4  x5-1 x3-1 x7+1 x2+1 
COD4  x5-1 x3-1 x1+1 x7+1 
  x3-1 x4+1 x1-1 x7+1 x2+1 
  x5-1 x4-1 x1+1 x7+1 x2-1 
  x5-1 x4-1 x1-1 x7+1 x2+1 
  x5-1 x3+1 x4-1 x1-1 x6-1 x2+1 
  x5-1 x3+1 x4-1 x6-1 x7-1 x2+1 
  x3+1 x4-1 x1+1 x6-1 x2-1 
  x3+1 x4-1 x1+1 x6-1 x7-1 
  x3+1 x4-1 x1-1 x6-1 x7+1 x2+1 
  x5-1 x3-1 x4-1 x1+1 x2-1 
  x5-1 x3-1 x1+1 x6-1 x2-1 
  x5+1 x3-1 x4+1 x1+1 x2-1 
  x3-1 x4+1 x1+1 x6-1 x2-1 
*}
C1 = C/(i -> map(R,i))
C2 = for i in C1 list saturate(ideal i, product flatten entries last topCoefficients i)
assert(intersect C2 == I) -- (FAILS) BUG: this should not fail (we think we are sure that I is radical!)
P = ideal (x5 + 1, x3 - 1, x1 + 1, x2 - 1) -- P is a minimal prime of I
assert((gens I) % P == 0)
-- one of the C2 components must (I think!) be P: (or be contained in P: i.e. C2 really should contain
-- the minimal primes of I).
for i in C2 list ((gens i) % P)
assert(# select(C2, i -> isSubset(i, P)) > 0) -- at least one of the primes in the list should contain P (FAILS)
assert(I == intersect decompose I) -- (FAILS)

