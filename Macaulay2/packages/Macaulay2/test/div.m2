R = degreesRing 1
t = R_0
h = 1-t^2-2*t^5-t^6+2*t^7+3*t^8-2*t^10
assert( h % (1-t) == 0 )
assert( (h // (1-t)) * (1-t) == h )

debug Core

S = QQ[x,y]/(x^2-5)
assert ( 1//x * x == 1 )
raw (1_S) // (raw x)

S = QQ[x]/(x^2-5)
assert ( 1//x * x == 1 )
raw (1_S) // (raw x)
use ambient S
x
rawExtendedGCD(raw x, raw(x^2-5))

R = QQ[x]
gcdCoefficients(x,x^2-5)
rawExtendedGCD(raw x, raw(x^2-5))

-- this never could have worked:
-- R = ZZ[x]
-- gcdCoefficients(x,x^2-5)
-- rawExtendedGCD(raw x, raw(x^2-5))

R = QQ[x,y]/(x^2-5)
1//x

R = QQ[x]/(x^2+1)[y]
r = 1_R % (x*1_R)
q = 1_R // (x*1_R)
assert(1_R - x*q - r == 0)
1_R % gb ideal (x*1_R)

remquottest = (f,g) -> (
     r = f % g;
     q = f // g;
     f - q*g - r)

S = ZZ[x,y,z]/(3*x^2-y-1)

assert(remquottest(1_S, x^2) == 0)
assert(remquottest(y^3, x+1) == 0)
(x+1)*q

T = ZZ[a]/(a^3-a-1)
A = T[x,y,z]/(x*a-1)

assert(remquottest(1,x) == 0)
assert(remquottest(1,x*y) == 0)
assert(remquottest(x*y,a+1) == 0)

B = GF(8,Variable=>w)[symbol x]/(x^3-w-1)
assert(remquottest(x+w, x^23) == 0)
