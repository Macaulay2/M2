--status: this old test depends on internal things and probably should be deleted


-----------------------------
-- Test of monoid creation --
-----------------------------
needs "raw-util.m2"
mo = rawMonomialOrdering { GRevLex => {1,2,3,4} }
M = rawMonoid(mo, (a,b,c,d)/toString, degring 2, (0,1, 0,1, 1,0, 1,0),{1,1})
R = rawPolynomialRing(rawZZ(), M)
a = rawRingVar(R,0)
c = rawRingVar(R,2)
rawMultiDegree (a*c^2)

-----------------------------
-- Test of GaloisField ------
-----------------------------
needs "raw-util.m2"
R = ZZ/5[x]
f = x^2-x+1
factor f
A = R/f
x = A_0
x^2
x^3
x^4
x^5
x^6
use R
a = 1
f = (x-a)^2-(x-a)+1
factor f
A = R/f
x = A_0
apply(1..24, i -> x^i)
f

needs "raw-util.m2"
R = ZZ/5[x]
f = x^2+2*x-2 -- x is primitive here
A = R/f
A' = raw A
B = rawGaloisField raw x
x = B_0
apply(1..24, i -> x^i)

GF 25
-----------------------------
-- Test of SchurRing --------
-----------------------------
needs "raw-util.m2"
needsPackage "SchurRings"
x = symbol x
R = schurRing(x,4)
f = x_{1,1}
dim f
g = f*f
assert(dim g == (dim f)^2)
<< "listForm of polynomials in Schur rings incorrect" << endl;
<< "display of polynomials in Schur rings incorrect" << endl;
listForm g -- INCORRECT
rawPairs(rawZZ(),raw g)
toString raw g
rawSchurDimension
S = ZZ[symbol a .. symbol d]
listForm(a^2+b^2+c^3)
exponents(a^2+b^2+c^3)
code(exponents,RingElement)
code(listForm,RingElement)

-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test/engine raw2.out"
-- End:
