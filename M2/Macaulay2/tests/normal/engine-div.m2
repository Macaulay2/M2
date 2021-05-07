-- Test of division in various rings
-- ggdiv, ggmod, ggdivmod.
-- For the purposes of this test, we define:
debug Core
quot = (m,n) -> new ring m from raw m // raw n
rem = (m,n) -> new ring m from raw m % raw n
remquot = (m,n) -> (
     R := ring m;
     t := rawDivMod(raw m, raw n);
     {new R from t#1, new R from t#0})

checkremquot = (m,n) -> (
     q := quot(m,n);
     r := rem(m,n);
     assert(m - (q*n+r) == 0);
     rq := remquot(m,n);
     assert(r == rq#0);
     assert(q == rq#1);
     )
    
-- Polynomials over the integers, one variable.
R = ZZ[x]

f = x^3-13*x^2-27
g = 13*x-5
rem(f,g)
quot(f,g)
checkremquot(f,g)

f = (x-3)*(2*x-5)*(13*x^2-x-1)^2
g = (x-3)*(2*x-5)
assert(quot(f,g) == (13*x^2-x-1)^2)
assert(rem(f,g) == 0)
checkremquot(f,g)

checkremquot(0_R,f)
checkremquot(f,f)
checkremquot(f+1,f)
checkremquot(f^2,f)
checkremquot(f,2*f)
remquot(f,2*f)

-- Polynomials over the integers, more variables
R = ZZ[x,y,z]
f = x^3*y*z-13*x^2-27*z^2+134
g = 13*x-5*y+7
remquot(f,g)
checkremquot(f,g)
checkremquot(g,f)
checkremquot(f*g,f)
checkremquot(f*g,g)
checkremquot(f*g+1,f)
checkremquot(f,2_R)
checkremquot(f,7_R)

-- Polynomials over a finite field
R = ZZ/7[x]

f = x^3-13*x^2-27
g = 13*x-5
rem(f,g)
quot(f,g)
checkremquot(f,g)

f = (x-3)*(2*x-5)*(13*x^2-x-1)^2
g = (x-3)*(2*x-5)
assert(quot(f,g) == (13*x^2-x-1)^2)
assert(rem(f,g) == 0)
checkremquot(f,g)

checkremquot(0_R,f)
checkremquot(f,f)
checkremquot(f+1,f)
checkremquot(f^2,f)
checkremquot(f,2*f)
remquot(f,2*f)

-- Polynomials over a finite field
R = GF(8)[x]
a = (coefficientRing R)_0
f = a*x^3-13*x^2-27
g = 13*x-5*a^3+1
rem(f,g)
quot(f,g)
checkremquot(f,g)

f = (x-a)*(2*x-5)*(13*x^2-x-a^2)^2
g = (x-a)*(2*x-5)
assert(quot(f,g) == (13*x^2-x-a^2)^2)
assert(rem(f,g) == 0)
checkremquot(f,g)

checkremquot(0_R,f)
checkremquot(f,f)
checkremquot(f+1,f)
checkremquot(f^2,f)

-- Polynomials in an algebraic extension
-- Currently not implemented:
R = QQ[x]/(x^2+1)[y]
remquot(1_R,x*1_R)

-- Polynomials in an algebraic extension
K = toField(QQ[x]/(x^2+1))
R = K[y]
checkremquot(1_R,x*1_R)
checkremquot(y^2+1, y-x)

R = QQ[x,y]/(x^3+x+1)
checkremquot(1_R,x)
checkremquot((x+y)^3,x)
checkremquot((y^2+x)^3,y+x)
checkremquot((y^2+x)^3,y^2+x)

-- Polynomials in a Laurent polynomial ring
R = ZZ[x,y,z,Inverses=>true, MonomialOrder=>RevLex]
f = 1 - x^-1
g = x-1
checkremquot(f,g)
checkremquot(g,f)
checkremquot(f^14,g)
f = 1 - 3*x^2 + 2*x^3
checkremquot(f,(1-x)^4)
g = quot(f,1-x)
g = quot(g,1-x)
rem(g,1-x)
checkremquot(f,1-x^-1)

--status: the engine goes into an infinite loop on this and fills memory
--status: should be fixed, because the engine should know whether an algorithm terminates before running it
checkremquot(f,1-x^-1-y^-1) -- HANGS

f % (1-x^-1)
f == (1-x^-1) * (f // (1-x^-1))
f % (1-x^-1-y^-1)

R = QQ[t,u,Inverses => true, MonomialOrder=>RevLex]

-- (1 - t^4) / (1 - t^2)
-- (1 + t^4) / (1 - t^2)
-- (1 - t^-4) / (1 - t^-2)
-- (1 + t^-4) / (1 - t^-2)
-- (1 - t^-4 * u^-4) / (1 - t^-2 * u^-2)
remquot((1 + t + t^-4 * u^-4), (1 - t^-2 * u^-2))
checkremquot((1 + t + t^-4 * u^-4), (1 - t^-2 * u^-2))

-- Polynomials in a skew commutative polynomial ring
R = QQ[symbol a..symbol d,SkewCommutative=>true]
f = a*b-c
g = a-3
rq = remquot(f,g)
(a-3)*(rq#1)
(rq#1)*(a-3)
checkremquot(f,g)

-- ZZ[vars]/I
-- These shouldn't work, but they do...
R = ZZ[x,y,z]/ideal(x^2+y^2+2*z^2,134)
f = x^2+1
g = x+1
checkremquot(f,g)
f = (x+y+z)^2*(x-y-1)
g = (x-y-1)
checkremquot(f,g)
checkremquot(x^2,y^2+2*z^2)

end
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test engine-div.out"
-- End:
