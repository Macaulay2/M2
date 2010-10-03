assert( ((setRandomSeed "hi there";)) === null )
assert( (debug Core) === null )
R = ZZ[x]
assert( (random(R^1,R^{-5})) === map(R^1,R^{{-5}},{{4*x^5}}) )
R = QQ[a,b,symbol x,symbol y,symbol z]
S = R/(x-y,a*x-b*z)
use R
assert( (pres = gens gb presentation S) === map(R^1,R^{{-1},{-2}},{{x-y, a*y-b*z}}) )
assert( (toString rawBasis(raw pres, {2}, {2}, {1}, {0,1,2,3,4}, false, -1)) === "a2 ab az b2 by bz y2 yz z2 
" )
R = QQ[a,b][symbol x,symbol y,symbol z]
S = R/(x-y,a*x-b*z)
use R
assert( (pres = gens gb presentation S) === map(R^1,R^{{-1,-1},{-1,0}},{{a*y-b*z, x-y}}) )
assert( (toString rawBasis(raw pres, {2,0}, {2,0}, {1,1}, {0,1,2,3,4}, false, -1)) === "y2 yz z2 
" )
assert( (toString rawBasis(raw pres, {2,1}, {2,1}, {1,1}, {0,1,2,3,4}, false, -1)) === "y2b yzb z2a z2b 
" )
assert( (toString rawBasis(raw pres, {2,2}, {2,2}, {1,1}, {0,1,2,3,4}, false, -1)) === "y2b2 yzb2 z2a2 z2ab z2b2 
" )
assert( (toString rawBasis(raw pres, {2,3}, {2,3}, {1,1}, {0,1,2,3,4}, false, -1)) === "y2b3 yzb3 z2a3 z2a2b z2ab2 z2b3 
" )
assert( (toString rawBasis(raw pres, {2,0}, {2,0}, {1,1}, {0,1,2}, false, -1)) === "y2 yz z2 
" )
assert( (toString rawBasis(raw pres, {3,0}, {3,0}, {1,1}, {0,1,2}, false, -1)) === "y3 y2z yz2 z3 
" )
assert( (toString rawBasis(raw pres, {0}, {0}, {1}, {0,1,2}, false, -1)) === "1 
" )
assert( (toString rawBasis(raw pres, {1}, {1}, {1}, {0,1,2}, false, -1)) === "y z 
" )
assert( (toString rawBasis(raw pres, {2}, {2}, {1}, {0,1,2}, false, -1)) === "y2 yz z2 
" )
assert( (toString rawBasis(raw pres, {3}, {3}, {1}, {0,1,2}, false, -1)) === "y3 y2z yz2 z3 
" )
assert( (basis_0 S) === map(S^1,S^1,{{1}}) )
assert( (basis_1 S) === map(S^1,S^{{-1,0},{-1,0}},{{y, z}}) )
assert( (basis_2 S) === map(S^1,S^{{-2,0},{-2,0},{-2,0}},{{y^2, y*z, z^2}}) )
assert( (basis_3 S) === map(S^1,S^{{-3,0},{-3,0},{-3,0},{-3,0}},{{y^3, y^2*z, y*z^2, z^3}}) )
R = QQ[a,b]
assert( (M = coker matrix transpose{{1,-1,0},{a,0,-b}}) === cokernel map(R^3,R^{{0},{-1}},{{1, a}, {-1, 0}, {0, -b}}) )
S = symmetricAlgebra M
use R
assert( (basis_0 S) === map(S^1,S^1,{{1}}) )
assert( (basis_1 S) === map(S^1,S^{{-1,0},{-1,0}},{{p_1, p_2}}) )
assert( (basis_2 S) === map(S^1,S^{{-2,0},{-2,0},{-2,0}},{{p_1^2, p_1*p_2, p_2^2}}) )
assert( (basis_3 S) === map(S^1,S^{{-3,0},{-3,0},{-3,0},{-3,0}},{{p_1^3, p_1^2*p_2, p_1*p_2^2, p_2^3}}) )
R = QQ[a]
assert( (basis(-4,-2,R)) === map(R^1,R^0,0) )
assert( (basis(-2,-4,R)) === map(R^1,R^0,0) )
assert( (basis(4,2,R)) === map(R^1,R^0,0) )
assert( (basis(2,4,R)) === map(R^1,R^{{-2},{-3},{-4}},{{a^2, a^3, a^4}}) )
R = QQ[a,Degrees => { -1 }]
assert( (basis(-4,-2,R)) === map(R^1,R^{{2},{3},{4}},{{a^2, a^3, a^4}}) )
assert( (basis(-2,-4,R)) === map(R^1,R^0,0) )
assert( (basis(4,2,R)) === map(R^1,R^0,0) )
assert( (basis(2,4,R)) === map(R^1,R^0,0) )

end

-- assertions above generated from this:

(setRandomSeed "hi there";)

debug Core

R = ZZ[x]
random(R^1,R^{-5})

R = QQ[a,b,symbol x,symbol y,symbol z]
S = R/(x-y,a*x-b*z)
use R
pres = gens gb presentation S
toString rawBasis(raw pres, {2}, {2}, {1}, {0,1,2,3,4}, false, -1)

R = QQ[a,b][symbol x,symbol y,symbol z]
S = R/(x-y,a*x-b*z)
use R
pres = gens gb presentation S
toString rawBasis(raw pres, {2,0}, {2,0}, {1,1}, {0,1,2,3,4}, false, -1)
toString rawBasis(raw pres, {2,1}, {2,1}, {1,1}, {0,1,2,3,4}, false, -1)
toString rawBasis(raw pres, {2,2}, {2,2}, {1,1}, {0,1,2,3,4}, false, -1)
toString rawBasis(raw pres, {2,3}, {2,3}, {1,1}, {0,1,2,3,4}, false, -1)

toString rawBasis(raw pres, {2,0}, {2,0}, {1,1}, {0,1,2}, false, -1)
toString rawBasis(raw pres, {3,0}, {3,0}, {1,1}, {0,1,2}, false, -1)

toString rawBasis(raw pres, {0}, {0}, {1,1}, {0,1,2}, false, -1)
toString rawBasis(raw pres, {1}, {1}, {1,1}, {0,1,2}, false, -1)
toString rawBasis(raw pres, {2}, {2}, {1,1}, {0,1,2}, false, -1)
toString rawBasis(raw pres, {3}, {3}, {1,1}, {0,1,2}, false, -1)

basis_0 S
basis_1 S
basis_2 S
basis_3 S

R = QQ[a,b]
M = coker matrix transpose{{1,-1,0},{a,0,-b}}
S = symmetricAlgebra M
use R
basis_0 S
basis_1 S
basis_2 S
basis_3 S

R = QQ[a]
basis(-4,-2,R)
basis(-2,-4,R)
basis(4,2,R)
basis(2,4,R)

R = QQ[a,Degrees => { -1 }]
basis(-4,-2,R)
basis(-2,-4,R)
basis(4,2,R)
basis(2,4,R)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages/Macaulay2Doc/test pushforward2.out"
-- End:
