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
