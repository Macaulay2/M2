-- test of routines from libfac and factory
-- and the resulting routines in factor.m2 (e.g. decompose)

-- 2 by 2 permanents of a 3 by 3.
S = ZZ/32003[a..i]
J = ideal(b*d+a*e, c*d+a*f, c*e+b*f, b*g+a*h, c*g+a*i, c*h+b*i, e*g+d*h, f*g+d*i, f*h+e*i)
errorDepth = 0
time C = irreducibleCharacteristicSeries J

-- note: this routine gives the WRONG answer if 0 is an argument
irreducibleCharacteristicSeries ideal(0_S,0_S,a)
-- these two are OK
irreducibleCharacteristicSeries ideal(a)
irreducibleCharacteristicSeries ideal(a, a^2, a^3)

topCoefficients C_0_0
assert(compress gens ideal(0_S,0_S,a,a+d,0_S,d) == matrix{{a,a+d,d}})
time decompose J
saturate(ideal(C_0_0), i, Strategy => Eliminate)


load "raw-util.m2"
rawIdealReorder raw gens J
rawCharSeries raw gens J

m = matrix{{a*(b-c)^2, a^3*(b+c^2)^3, a*b*c*d}}
divideByVariable(m,a)
divideByVariable(m,a,4)
saturate(ideal m, a)
///
{a, b, c, d*g + e*i, f, h}
{a, b, c, d, h, i}
{b*e + a*f, c, d, g, h, i}
{a, b, c, d*f + e*h, g, i}
{b, c, f, g, h, i}
{c*e + a*g, b, d, f, h, i}
{b*d + a*h, c, e, f, g, i}
{a, b, c, e, f, g}
{c*d + a*i, b, e, f, g, h}
{a, b, d, e, f, h}
{a, c*h + b*i, d, e, f, g}
{a, c*f + b*g, d, e, h, i}
{a, c, d, e, g, i}
{d, e, f, g, h, i}
{a, b, c, d, e, g*h + f*i}
///

restart
S = ZZ/32003[a..i]
m = matrix{{a*(b-c)^2, a^3*(b+c^2)^3, a*b*c*d}}
J = ideal m
time Ja = saturate(J, a, Strategy=>Eliminate);
assert(Ja == J : a^3)
saturate(J, a, Strategy=>Iterate) -- quotients not allowed
-- the quotients below seem to work
J1 = J : a
J2 = J1 : a
J1 == J2
J3 = J2 : a
J3 == J2
J4 = J3 : a
assert(J4 == J3)
mingens J4

restart
S = ZZ/32003[a..i]
m = matrix{{a*(b-c)^2, a^3*(b+c^2)^3, a*b*c*d}}
J = ideal m

restart
A = ZZ/32003[a..d]
B = ZZ/32003[Variables=>5, MonomialOrder=>Eliminate 1,MonomialSize=>16]
F = map(B,A,genericMatrix(B,B_1,1,4))
F a
errorDepth = 0

--------------------------
restart
needs "raw-util.m2"
R = ZZ/32003[symbol a..symbol d]
I = ideal(a*d-b^2-b-1, c^3-c-a)
J = ideal(a*b*c-3*a-1, b^3-a*d-a-1)
L = intersect(I,J);
L1 = ideal gens gb L
L == L1 -- false
L1_0
assert(rawGBContains(raw gb L, raw gens L1) == -1)
assert(rawGBContains(raw gb L, raw matrix{{L1_0}}) == -1)
assert(rawGBContains(raw gb L1, raw gens L) == -1) -- this one is OK.

Lanswer = ideal(a*b^3*c-a^2*b*c*d+a*b^2*c-3*a*b^2+a*b*c+3*a^2*d-3*a*b-b^2+a*d-3*a-b-1,
     b^5-a*b^3*d+b^4-a*b^2*d+a^2*d^2-a*b^2+b^3+a^2*d-a*b*d-a*b-b^2-a-b-1,
     a^2*b^2*c*d-a^3*c*d^2-3*a*b^4+a^2*b^2*c-a*b^3*c+3*a^2*b^2*d-a^3*c*d+2*a^2*b*c*d-3*a*b^3-b^4+a^2*b*c+a*b^2*d-b^3+a^2*c-3*a^2*d+3*a*b+a*c-a*d+3*a+b+1,
     a*b*c^4-a^2*b*c-a*b*c^2-3*a*c^3-c^3+3*a^2+3*a*c+a+c,
     b^3*c^3-10668*a*b*c^4-a*c^3*d-a*b^3+10668*a^2*b*c-b^3*c+10668*a*b*c^2+10667*c^3+a^2*d+a*c*d-10667*a-10667*c,
     a^2*c^4*d-11644*a*b*c^4*d-3*a*b^2*c^3-8697*b^3*c^3+a^2*c^4+2903*a*b*c^4-b^2*c^3+a*c^4-a^3*c*d+11644*a^2*b*c*d-a^2*c^2*d+11644*a*b*c^2*d+11626*a*c^3*d+3*a^2*b^2+8697*a*b^3-a^3*c-2903*a^2*b*c+3*a*b^2*c+8697*b^3*c-a^2*c^2-2903*a*b*c^2-12*a*c^3+11644*c^3*d+a*b^2-a^2*c+b^2*c-a*c^2+5794*c^3-11626*a^2*d-11626*a*c*d+12*a^2+12*a*c-11644*a*d-11644*c*d-5794*a-5794*c,
     a^2*b*c^3*d-a*b^2*c^3-a^2*c^3*d-a^2*b^3+3*a*b^2*c^2-a^2*c^3-a*b*c^3-a^2*b*c*d-3*a^2*c^2*d+a*b^2*c+3*a*b*c^2+b^2*c^2-a*c^3+a^3*d+a^2*c*d-a*c^2*d+a^3-3*a*b^2+a^2*c+a*b*c+3*a*c^2+b*c^2+3*a^2*d+a^2-3*a*b-b^2+a*c+c^2+a*d-3*a-b-1,
     a^3*c^3*d^2-896*a*b*c^4*d^2-a^2*b^2*c^3+560*a*b^2*c^4-1680*b^3*c^4+a^3*c^3*d+2*a^2*b*c^3*d+3080*b^3*c^3*d-252*a^2*c^4*d-1232*a*b*c^4*d-a^2*b^4+b^4*c^2-2*a^2*b*c^3+750*a*b^2*c^3-2604*b^3*c^3-252*a^2*c^4+2632*a*b*c^4-a*b^2*c^2*d-5*a^2*c^3*d+1680*a*c^4*d-a^3*c*d^2+896*a^2*b*c*d^2+896*a*b*c^2*d^2-392*a*c^3*d^2-5*a^2*b^3-3*a*b^4-559*a^2*b^2*c-548*a*b^2*c^2+1682*b^3*c^2-6*a^2*c^3-1686*a*b*c^3+252*b^2*c^3+1428*a*c^4+a^3*b*d+3*a^2*b^2*d-3080*a*b^3*d+251*a^3*c*d+2910*a^2*b*c*d-3080*b^3*c*d+240*a^2*c^2*d+1231*a*b*c^2*d+3220*a*c^3*d+896*c^3*d^2+a^3*b-756*a^2*b^2+2598*a*b^3-b^4+252*a^3*c-2630*a^2*b*c-2421*a*b^2*c+2607*b^3*c+252*a^2*c^2-2620*a*b*c^2+6*b^2*c^2-5298*a*c^3-560*b*c^3+1680*c^4+5*a^3*d+3*a^2*b*d+a*b^2*d-1684*a^2*c*d-3*a*b*c*d-1684*a*c^2*d-1848*c^3*d+392*a^2*d^2+392*a*c*d^2+5*a^3+1681*a^2*b+4797*a*b^2-2*b^3-1422*a^2*c+15*a*b*c-246*b^2*c-1416*a*c^2+5*b*c^2-28*c^3-8275*a^2*d+a*b*d-3223*a*c*d-896*a*d^2-896*c*d^2+5297*a^2+5612*a*b+1683*b^2+3627*a*c+566*b*c-1676*c^2+163*a*d+1848*c*d+5083*a+1684*b+31*c+1685)
assert(L == Lanswer)
L1 = ideal gens gb L;
Lanswer_0 == L1_0
Lanswer_1 == L1_1
Lanswer_2 + L1 _0 == L1_2
Lanswer_3 == L1_3
Lanswer_4 + 10668*L1_3 == L1_4
Lanswer_5 + 11644*d*L1_3 + 8697*L1_4 - 2903*L1_3 == L1_5
Lanswer_6 == L1_6
Lanswer_7 + 896*d^2*L1_3 - 560*b*L1_3 + 1680*c*L1_4 -2*L1_6 - 3080*d*L1_4 + 252*L1_5 + 1232*d*L1_3 + 2604*L1_4 - 2632*L1_3 + 1680*L1_0 == L1_7
assert(Lanswer == L1) -- true
assert(Lanswer == L)
assert(L1 == L)

print "The following decompose takes too long, so it is commented out" 
decompose ideal mingens L1 -- ouch!
irreducibleCharacteristicSeries L1

apply(numgens Lanswer, i -> Lanswer_i == L1_i)
gbTrace = 3
gens gb L;
ideal gens gb L
Lmin = ideal(L_0)
scan(numgens L - 1, i -> (
	  L1 := Lmin + ideal(L_(i+1));
	  if Lmin != L1
	  then Lmin = L1))
transpose gens Lmin
syz gens Lmin
Lmin == L
Lmin = ideal(Lmin_0, Lmin_2, Lmin_4, Lmin_5, Lmin_6)
Lmin == L

-- decompose interrupt bug
restart
R = ZZ/32003[symbol a..symbol d]
I = ideal(a*d-b^2-b-1, c^3-c-a)
J = ideal(a*b*c-3*a-1, b^3-a*d-a-1)
L = intersect(I,J);
L1 = ideal gens gb L
irreducibleCharacteristicSeries ideal(a,b)
M = L1^3; -- NOW interrupt this
irreducibleCharacteristicSeries ideal(a,b)


--irreducibleCharacteristicSeries L1
