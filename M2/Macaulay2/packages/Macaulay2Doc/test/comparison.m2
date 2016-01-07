R = QQ[x]
x-x ? x-x
m = matrix{{x^3, x^2-1, 0_R, x, 0_R, 1_R, 2_R}}
L1 = sort first entries m
m1 = sort m
assert( L1 === first entries m1 )			    -- the two sorting routines should be compatible

R = ZZ[x,y,z]
L = {12*x, 3*x-4, 3*x-5, x^2+1, -x^3+3}
sort L
L = {12_R, 0_R, 14_R, -3_R}
sort L

Q = frac R
L = {1/x, 1/y, 14*x^3/y^2, x^2/y, x^3/y, -14_Q}
assert(sort L === {-14_Q, 1/(y), 1/(x), (x^2)/(y), (x^3)/(y), (14*x^3)/(y^2)})

-- Ordering in ZZ/p:
-- This order isn't perfect, but it is the way we have done it now for some time.
-- Changing it causes a number of tests to fail, since the sort order affects many things.
-- So we are leaving this as is (MES 27 Oct 2015).
assert(
    sort apply (11, i -> mod(i,11))
    ==
    {-1, -2, -3, -4, -5, 5, 4, 3, 2, 1, 0}
    )

-- Some more comparisons
debug Core
R = ZZ[x]
3784237846234783624873*x ? 549857983578349573489573895738957389*x

R = QQ[x,y,z]
f0 = 9264084868888584706422920457436045097105319764589241237356250598400000000
g0 = 26037185781611888053803981844062336
assert(f0 > g0)
assert(f0*x > g0*x) -- error
f = 9264084868888584706422920457436045097105319764589241237356250598400000000*z
g = 26037185781611888053803981844062336*z
assert((f ? g) == symbol>)
assert(1 == rawCompare(raw f, raw g)) -- gives wrong answer (4)
rawCompare(raw 9264084868888584706422920457436045097105319764589241237356250598400000000_R, raw 26037185781611888053803981844062336_R)
f > g -- also gives wrong answer.

debug Core
kk = ZZp(101)
R = kk[x,y,z]
assert(rawCompare(raw(3*x), raw(5*x)) == 1)
for i from 0 to 100 list (i*x)
sort oo

kk = ZZp(101, Strategy=>"Old")
R = kk[x,y,z]
rawCompare(raw(3*x), raw(5*x)) == 1
for i from 0 to 100 list (i*x)
sort oo

kk = ZZp(101, Strategy=>"Aring")
R = kk[x,y,z]
rawCompare(raw(3*x), raw(5*x)) == -1
for i from 0 to 100 list (i*x)
sort oo

R1 = ZZ[a,b,c]
R2 = QQ[d,e]
-- a > d -- can't compare them
assert(((a^9) ? (3483274892374893274238974238947238972389742389473289472389472893482*a^9)) == symbol<)

-- the following test returns 0, as documentation states.  But it will never be used in the front end,
-- as methods are installed for comparison for each ring (not for elements between rings).
assert(0 == rawCompare(raw a, raw d))
  