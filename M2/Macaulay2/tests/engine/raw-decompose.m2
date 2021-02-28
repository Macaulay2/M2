--status: this old test depends on internal things and probably should be deleted


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
saturate(ideal(C_1 C_0_0), i, Strategy => Eliminate)


needs "raw-util.m2"
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

S = ZZ/32003[a..i]
m = matrix{{a*(b-c)^2, a^3*(b+c^2)^3, a*b*c*d}}
J = ideal m

A = ZZ/32003[a..d]
B = ZZ/32003[Variables=>5, MonomialOrder=>Eliminate 1,MonomialSize=>16]
F = map(B,A,genericMatrix(B,B_1,1,4))
F a
errorDepth = 0

--------------------------
needs "raw-util.m2"
R = ZZ/32003[symbol a..symbol d]
I = ideal(a*d-b^2-b-1, c^3-c-a)
J = ideal(a*b*c-3*a-1, b^3-a*d-a-1)
L = intersect(I,J);
L1 = ideal gens gb L
assert(L == L1)

stderr << "The following decompose takes too long, so it is commented out" << endl
time C = decompose ideal mingens L1 -- ouch!
assert(set(C/(I -> flatten entries gens gb I)) === set({I,J}/(i -> flatten entries gens gb i)))



