-- Test of some simple GB things
-- over quotient rings

-----------------------
R = ZZ/101[x]/(x^3-x-1)
J = ideal(x)
gbTrace = 3 
g = gb(J,ChangeMatrix=>true)
gens g
getChangeMatrix g
m = matrix{{1_R}}
assert(m // (gens J) - matrix{{x^2-1}} == 0)
assert(x^2-1 == first first entries ((1_R) // (gens J)))
assert(id_(R^1) == matrix{{1_R}})
-----------------------
R = ZZ/101[symbol x, symbol y, symbol z, symbol w]
A = R/ideal(x*w-y*z, y^2-x*z)
x
P = vars A
syz P

use R
A = R/ideal(x^2+y^2, z^2+w^2)
P = vars A
P1 = syz P
P2 = syz P1
P3 = syz P2
P4 = syz P3
assert(P * P1 == 0)
assert(P1 * P2 == 0)
assert(P2 * P3 == 0)
assert(P3 * P4 == 0)
-----------------------
-- ideal quotients use quotient rings often
R = ZZ/101[a..d]
I = intersect(ideal(a*d-b*c, a^3-b-1, b^3-a-1), ideal(a-3,b-1,c-1,d-2))
ans = ideal(-20*b*c*d+20*a*d^2+40*b*c-40*a*d,
     -20*b*c^2+20*a*c*d+20*b*c-20*a*d,
     -20*b^2*c+20*a*b*d+20*b*c-20*a*d,
     -20*a*b*c+20*a^2*d-41*b*c+41*a*d,
     20*b^4*c-20*a*b^3*d-b^3-20*a*b*c+20*a^2*d-20*b*c+20*a*d+a+1,
     20*a^3*b*c-20*a^4*d-a^3-20*b^2*c+20*a*b*d-20*b*c+20*a*d+b+1)
assert(ans == I)
transpose mingens I
assert(ideal mingens I == I)

R = ZZ/101[a..d]
I = ideal(20*b*c*d-20*a*d^2-40*b*c+40*a*d,20*b*c^2-20*a*c*d-20*b*c+20*a*d,20*b^2*c-20*a*b*d-20*b*c+20*a*d,20*a*b*c-20*a^2*d+41*b*c-41*a*d,-b^3+41*b*c-41*a*d+a+1,-a^3-5*b*c+5*a*d+b+1)
J = ideal(a-3)
A = (ring I)/I
m = transpose gens J ** A
gb m
syz m
mingens ideal oo

use ring I
time(J = I : (a-3))
  ans = ideal(b*c-a*d,
       a^2*c-b^2*d+c-d,
       b^3-a-1,
       a^3-24*a^2*c+24*b^2*d-b-24*c+24*d-1,
       a*b*d^2-b^2*d^2-21*a^2*c+a*c^2+21*b^2*d-b*d^2-c^2+c*d-21*c+21*d,
       a*b^2*d+44*b^3*d-21*a^3+3*a^2*c-3*b^2*d-a*c-44*a*d+21*b+2*c-47*d+21,
       a*c^3-26*a^3*d+41*a^2*c*d-41*b^2*d^2-b*d^3-35*a^3+5*a^2*c+c^3-5*b^2*d-d^3+26*b*d+41*c*d-41*d^2+35*b+5*c+21*d+35,
       a^2*d^3-b^2*d^3-21*a^2*c^2+a*c^3-21*a^2*c*d+a*c^2*d+42*b^2*d^2-a*d^3-b*d^3+37*a^2*c-21*a*c^2-c^3-37*b^2*d+21*b*d^2+c*d^2-21*c*d+21*d^2+37*c-37*d,
       a*c^2*d^2-a*c*d^3-2*c^4-c^3*d+c*d^3+2*d^4,
       a*c*d^4-b*d^5+2*c^5+3*c^4*d+2*c^3*d^2-c^2*d^3-3*c*d^4-3*d^5,
       a*d^6-b*d^6-2*c^6-c^5*d-c^4*d^2+2*c^3*d^3+2*c^2*d^4+c*d^5-d^6,
       c^7-49*c^6*d+2*c^5*d^2+c^4*d^3-c^3*d^4-2*c^2*d^5+49*c*d^6-d^7)
assert(I : (a-3) == ans)

transpose gens gb J


-----------------------
-- now over ZZ
R = ZZ[a..d]
I = intersect(ideal(a*d-b*c, a^3-b-1, b^3-a-1), ideal(a-3,b-1,c-1,d-2))
ans = ideal(-b*c*d+a*d^2+2*b*c-2*a*d,
     -b*c^2+a*c*d+b*c-a*d,
     -b^2*c+a*b*d+b*c-a*d,
     -a*b*c+a^2*d+3*b*c-3*a*d,
     -a^3-5*b*c+5*a*d+b+1,
     5*b^3-3*b*c+3*a*d-5*a-5,
     -5*b^3+3*b*c-3*a*d+5*a+5,
     -2*b^3*d+4*b^3+b*c*d-a*d^2-2*b*c+4*a*d-4*a+2*d-4,
     -2*b^3*c+2*b^3+b*c^2-a*c*d+2*a*c-b*c+a*d-2*a+2*c-2,
     -2*b^4+2*b^3+b^2*c-a*b*d+2*a*b-b*c+a*d-2*a+2*b-2,
     -2*a*b^3+6*b^3+a*b*c-a^2*d+2*a^2-3*b*c+3*a*d-4*a-6) -- I think
assert(ans == ideal mingens I)
transpose mingens I


J = I : (a-3);
transpose mingens J
transpose gens gb J
assert(J == ideal(a*d-b*c, a^3-b-1, b^3-a-1))

A = (ring I)/I
m = transpose gens ideal(a-3) ** A
gb m
syzm = syz m 
syzm = syz m -- crashes the second time through?
mingens ideal oo 
-----------------------
R = ZZ/101 [X_0, X_1, X_2, X_3, X_4, X_5, Degrees => {{1}, {1}, {4}, {4}, {4}, {4}}, MonomialOrder => Eliminate{2}, MonomialSize => 16]
J = matrix {{-X_0^4+X_2, -X_0^3*X_1+X_3, -X_0*X_1^3+X_4, -X_1^4+X_5}}
gens gb J
assert(numgens source gens gb J == 14)
