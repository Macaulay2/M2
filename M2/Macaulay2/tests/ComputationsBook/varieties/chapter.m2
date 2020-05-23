R = QQ[x,y,z]
curve = ideal( x^4-y^5, x^3-y^7 )
gb curve
dim curve
codim curve
degree curve
curve1 = saturate(curve,ideal(x))
curve2 = saturate(curve,curve1)
curve == radical curve
curve = curve1
degree curve
curve == radical curve
surface = ideal( x^5 + y^5 + z^5 - 1)
theirunion = intersect(curve,surface)
curve*surface == theirunion
ourpoints = curve + surface
dim ourpoints
degree ourpoints
degree radical ourpoints
staircase = ideal leadTerm ourpoints
T = R/staircase;
basis T
use R;
anyOldPolynomial = y^5*x^5-x^9-y^8+y^3*x^5
anyOldPolynomial % ourpoints
anotherPolynomial = y^5*x^5-x^9-y^8+y^3*x^4
anotherPolynomial % ourpoints
R' = ZZ/101[x,y,z];
ourpoints' = substitute(ourpoints,R')
decompose ourpoints'
oo / print @@ print;
ooo / degree
S = QQ[z,y,x, MonomialOrder => Eliminate 2]
ourpoints'' = substitute(ourpoints,S)
G = gens gb ourpoints''
ideal selectInSubring(1,G)
M = staircase^3
numgens M
transpose gens M
degree M
S = R/M
basis S
tally apply(flatten entries basis(S),degree)
basis(19,S)
(x+y+z)^19
C = res M
C.dd_1
set flatten entries gens M - set flatten entries C.dd_1
C.dd_2
C.dd_3
A = {{1, 1, 1, 1},
     {1, 5,10,25}}
R = QQ[p,n,d,q, Degrees => transpose A]
degree d
degree q
degree(p^4*n^8*d^10*q^3)
h = basis({25,219}, R)
rank source h
rank source basis({100,1000}, R)
S = QQ[x, y, d, p, n, q, 
    MonomialOrder => Lex, MonomialSize => 16]
I = ideal( p - x*y, n - x*y^5, d - x*y^10, q - x*y^25)
transpose gens gb I
S' = S/I
x^10 * y^100
x^100 * y^1000
x^39 * y^1000
weight = (5,7,13,17)
T = QQ[x, y, p, n, d, q, 
          Weights => {{1,1,0,0,0,0},{0,0,weight}},
          MonomialSize => 16]/
      (p - x*y, n - x*y^5, d - x*y^10, q - x*y^25);
x^10 * y^100
x^100 * y^1000
x^234 * y^5677
