F = GF(81,Variable=>a)
a^80
a^40
ambient F
a^4 + a - 1
lift(a^20, ambient F)
apply({20,40,80}, i -> lift(a^i, ambient F))
R = F[x,y,z]
f = random(2,R)
f = (leadCoefficient f)^(-1) * f
ZZ/101
k = GF 81
k_0
a = k_0
a^20+1
ambient k
ideal oo
oo_0
F = GF(16, Variable => b)
b^20 + 1
random F
R = F[x,y,z]
random(2,R)
GF (ZZ/2[T]/(T^9+T+1), Variable => T)
A = ZZ/2[T]/(T^9+T+1)
k = GF (A, PrimitiveElement => T^3+1)
T
substitute(T,k)
lift(k_0, ring T)
lift(k_0, ambient ring T)
