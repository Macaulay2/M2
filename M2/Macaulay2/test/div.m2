R = degreesRing 1
t = R_0
h = 1-t^2-2*t^5-t^6+2*t^7+3*t^8-2*t^10
assert( h % (1-t) == 0 )
assert( (h // (1-t)) * (1-t) == h )

debug Macaulay2Core

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

R = ZZ[x]
gcdCoefficients(x,x^2-5)
rawExtendedGCD(raw x, raw(x^2-5))

R = QQ[x,y]/(x^2-5)
1//x
