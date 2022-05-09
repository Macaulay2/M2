-- -*- coding: utf-8 -*-
------------------------------------------------------------------------------
-- Copyright 2006--2020 Sorin Popescu, Gregory G. Smith, and Mike Stillman
--
-- This program is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by the Free
-- Software Foundation, either version 3 of the License, or (at your option)
-- any later version.
--
-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
-- FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
-- more details.
--
-- You should have received a copy of the GNU General Public License along
-- with this program.  If not, see <http://www.gnu.org/licenses/>.
------------------------------------------------------------------------------
-- Simplicial Complexes Tests
------------------------------------------------------------------------------
-*
restart
needsPackage "SimplicialComplexes"
*-
TEST ///
S = QQ[x_{1,1}..x_{2,4}, Degrees => {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}]
I = monomialIdeal(x_{1,1}*x_{2,1},x_{1,2}*x_{2,2},x_{1,3}*x_{2,3},x_{1,4}*x_{2,4})
D = simplicialComplex(I)
assert( (fVector(D))#3 == 32)
-- assert( (fVector(D, Flag => true))#{1,1,0,0} == 4)
///

------------------------------------------------------------------------------
-- Test
TEST ///
R1 = QQ[x,y,z];
S1 = simplicialComplex {x*y*z}
R2 = QQ[a,b];
S2 = simplicialComplex {a,b}
J = S1 * S2
assert((fVector(S1 * S2))#2 == (fVector(S1))#2 + (fVector(S1))#1*(fVector(S2))#1)
assert(star(J, sub(x*y*z, ring J)) === J)
assert(#(facets(star(J, sub(a, ring J)))) == 1)
///

------------------------------------------------------------------------------
-- Real Projective plane
TEST ///
R = ZZ[a..f];
RP2 = simplicialComplex monomialIdeal(a*b*c,a*b*f,a*c*e,a*d*e,a*d*f,b*c*d,b*d*e,b*e*f,c*d*f,c*e*f)
skel = skeleton(1, RP2)
-- removing facets creates holes captured by HH_1
assert(rank HH_1 skel == (fVector(RP2))#3)
assert(dim skel == 1)
S = ZZ[v]
v = simplicialComplex {v}
-- the cone is contractible
conewrtv = v * RP2
assert(prune HH_1 conewrtv == 0)
///

------------------------------------------------------------------------------
-- flagfVector for a colored complex
TEST ///
restart
needsPackage"SimplicialComplexes"
R = QQ[a .. e, Degrees => {{1,0,0,0},{0,1,0,0},{1,0,0,0},{0,0,0,1},{0,1,0,0}}]
D = simplicialComplex{a*b, b*c, c*d*e, a*e}
assert isProper D
assert(flagfVector({0,1,0,1}, D) == 1)
assert(flagfVector({0,1,0,0}, D) == 2)
assert(flagfVector({1,1,0,0}, D) == 4)
ffV = hashTable flatten for i from -1 to 2 list(
    for F in faces(i, D) list degree F => flagfVector(degree F, D)
    )
assert(ffV === flagfVector D)
-- degenerate cases
void = simplicialComplex monomialIdeal(1_R)
irrelevant = simplicialComplex monomialIdeal gens R
assert(flagfVector void === hashTable{})
assert(flagfVector irrelevant === hashTable{{0,0,0,0} => 1})
assert isProper void
assert isProper irrelevant
///

------------------------------------------------------------------------------
-- Example from Betti numbers of strongly color-stable ideals and
-- squarefree strongly color-stable ideals
-- Satoshi Murai
TEST ///
grading = {{1,0,0},{1,0,0},{1,0,0},{0,1,0},{0,0,1}}
R = QQ[x_{1,1},x_{1,2},x_{1,3},x_{2,1},x_{3,1}, Degrees => grading];
delta = simplicialComplex({x_{1,3}*x_{2,1}*x_{3,1},x_{1,1}*x_{2,1},x_{1,2}*x_{3,1}})
shifted = algebraicShifting(delta, Multigrading => true)
assert((fVector(delta))#0 == (fVector(shifted))#0)
assert((fVector(delta))#1 == (fVector(shifted))#1)
assert((fVector(delta))#2 == (fVector(shifted))#2)
assert(prune homology delta != prune homology shifted)
///

------------------------------------------------------------------------------
-- Boundary of the 4-Cross-Polytope
TEST ///
grading = {{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1},{1,0,0,0},{0,1,0,0},{0,0,1,0},{0,0,0,1}}
S = QQ[x_{1,1}..x_{2,4}, Degrees => grading];
I = monomialIdeal(x_{1,1}*x_{2,1},x_{1,2}*x_{2,2},x_{1,3}*x_{2,3},x_{1,4}*x_{2,4})
cross = simplicialComplex(I)
assert( (fVector(cross))#3 == 32)
-- assert( (fVector(cross, Flag => true))#{1,1,0,0} == 4)
assert(dim skeleton(2,cross) == 2)
assert((fVector(skeleton(2,cross)))#2 == (fVector(cross))#2)
multishifted = algebraicShifting(cross, Multigrading => true)
stdshifted = algebraicShifting(cross)
assert (cross === multishifted)
assert (cross =!= stdshifted)
///


------------------------------------------------------------------------------
-- Cartwright-Sturmfels ideals associated to graphs and linear spaces 
-- Aldo Conca, Emanuela De Negri, Elisa Gorla    Ex. 1.10
TEST ///
needsPackage "GenericInitialIdeal"
row_grading = {{1,0},{1,0},{1,0},{0,1},{0,1},{0,1}}
S=QQ[x_{1,1}..x_{2,3}, Degrees => row_grading];
I = ideal(x_{1,1}*x_{2,1},x_{1,2}*x_{2,2},x_{1,3}*x_{2,2},x_{1,2}*x_{2,3},x_{1,3}*x_{2,3})
multigin = ideal(x_{1,1}^2*x_{2,3},x_{1,1}*x_{1,2}*x_{2,3},x_{1,1}*x_{2,1},x_{1,2}*x_{2,1},x_{1,3}*x_{2,1},x_{1,1}*x_{2,2},x_{1,2}*x_{2,2})
stdgin = ideal(x_{1,1}^2,x_{1,1}*x_{1,2},x_{1,2}^2,x_{1,1}*x_{1,3},x_{1,2}*x_{1,3},x_{1,3}^3,x_{1,3}^2*x_{2,1})
assert(gin(I, AttemptCount => 10, Multigraded => true) == multigin)
assert(gin(I, AttemptCount => 10) == stdgin)
///

------------------------------------------------------------------------------
-- Stacked 3-sphere on 7 vertices
TEST ///
S = QQ[x_1..x_7];
I = monomialIdeal(x_2*x_3*x_4*x_5,x_3*x_4*x_5*x_6,x_1*x_6,x_1*x_7,x_2*x_7)
st73 = simplicialComplex I
shifted = algebraicShifting (st73)
assert(fVector st73 === fVector shifted)
assert(prune homology st73 == prune homology shifted)
assert(not member(x_1*x_2*x_3*x_4, facets(shifted)))
///


------------------------------------------------------------------------------
-- Test
TEST///
S = QQ[a..f];
D = simplicialComplex({a*b*c,b*c*d,d*e*f})
stD = star(D, d)
assert(star(D, d) === simplicialComplex({d*e*f,b*c*d}))
v = getSymbol "v"
T = QQ[v]
conev = stD * simplicialComplex({v})
assert(fVector conev === {1,6,11,8,2})
///


------------------------------------------------------------------------------
-- Testing void and irrelevant complexes.
TEST ///
R = ZZ[x];
void = simplicialComplex monomialIdeal(1_R)
assert isPure void
assert(dim void == -infinity)
assert(faces(0,void) == {})
assert(faces(-1,void) == {})
assert(vertices void == {})
C = chainComplex void
assert(C.dd^2 == 0 )
assert(HH_0(void) == 0)
assert(HH_-1(void) == 0)
assert(fVector void === {0})

irrelevant = simplicialComplex monomialIdeal gens R
assert isPure irrelevant
assert(dim irrelevant === -1)
assert(faces(0,irrelevant) == {})
assert(#faces(-1,irrelevant) === 1)
assert(irrelevant === dual irrelevant)
assert(fVector irrelevant === {1})
C = chainComplex irrelevant
assert(C.dd^2 == 0 )
assert(HH_0(irrelevant) == 0)
assert(HH_-1(irrelevant) == (coefficientRing R)^1)
assert try (simplicialComplex {};false) else true

voidToIrrelevant = map (irrelevant, void, gens R)
assert isWellDefined voidToIrrelevant
irrelevantToVoid = map(void, irrelevant, gens R)
-- the empty face has nowhere to go
assert not isWellDefined irrelevantToVoid

pvoid = prune void
assert isPure pvoid
assert(dim pvoid == -infinity)
assert(faces(0,pvoid) == {})
assert(faces(-1,pvoid) == {})
assert(vertices pvoid == {})

pirrelevant = prune irrelevant
assert isPure irrelevant
assert(dim irrelevant === -1)
assert(faces(0,irrelevant) == {})
assert(#faces(-1,irrelevant) === 1)
assert(irrelevant === dual irrelevant)
///


------------------------------------------------------------------------------
-- Miller and Sturmfels, example 1.8 
TEST ///
R = ZZ[a..e];
D = simplicialComplex monomialIdeal(a*d, a*e, b*c*d, d*e, c*e, b*e)
assert not isPure D
assert ( ideal dual D == monomialIdeal (a*b*c*d, a*b*e, a*c*e, d*e) )
S = ZZ/32003[u,v,w,x,y];
C = chainComplex(D, Labels => {u,v,w,x,y})
assert(C.dd^2 == 0 )
H = prune HH(C)
assert(H_0 != 0)
assert(H_1 != 0)
assert(rank H_2 == 1)
assert(H_3 == 0)
///


------------------------------------------------------------------------------
-- torus  : Munkres page 15 example 3 
TEST ///
R = QQ[a..j];
D = simplicialComplex{a*b*i, a*e*i, i*b*j, j*c*b, j*c*a, j*a*e,
     e*i*f, i*h*f, i*h*j, j*e*d, j*g*d, j*h*g, g*h*f, f*e*d,
     d*f*a, f*b*a, f*g*c, f*b*c, g*c*a, g*d*a}
assert isPure D
C = chainComplex D
assert( C.dd^2 == 0 )
H = prune HH(C)
assert(H_0 == 0)
assert(rank H_1 == 2)
assert(rank H_2 == 1)
D' = dual D
C' = chainComplex D'
assert(C'.dd^2 == 0 )
H' = prune HH(C')
assert (H'_(7-2) === H_2)
assert (H'_(7-1) === H_1)
///

------------------------------------------------------------------------------
-- Klein bottle : Munkres page 18 example 5 
TEST ///
R = ZZ/2[a..k];
D = simplicialComplex {a*b*i, a*e*i, b*i*j, b*c*j, a*c*j, 
     a*d*j, e*f*i, f*h*i, h*i*j, d*e*j, e*g*j, g*h*j, 
     f*g*h, d*e*f, a*d*f, a*b*f, c*f*g, b*c*f, a*c*g, a*e*g}
isPure D
assert(vertices D == toList(a..j))
C = chainComplex D
assert(C.dd^2 == 0)
H = prune HH(C)
assert(H_0 == 0)
assert(rank H_1 == 2)
assert(rank H_2 == 1)
///


------------------------------------------------------------------------------
-- Degenerations of Abelian surfaces 
-- Gross and Popescu, math.AG/9609001
TEST ///
abelian = (n) -> (
     R := QQ[local x_0..local x_(n-1)];
     L1 = toList apply(0..n-1, i -> x_i * x_((i+3)%n) * x_((i+4)%n));
     L2 = toList apply(0..n-1, i -> x_i * x_((i+1)%n) * x_((i+4)%n));
    join(L1,L2))

D = simplicialComplex abelian 8
#faces(0,D)
#faces(1,D)
#faces(2,D)
#faces(3,D)
C = chainComplex D
assert(C.dd^2 == 0)
prune HH(C)
transpose gens ideal D     
fVector D
///


------------------------------------------------------------------------------
-- Simplex with labelling 
TEST ///
R = ZZ[a..f];
D = simplicialComplex{product toList (b..f)}
assert(vertices D == toList(b..f))
assert(facets dual D == facets D)
S = ZZ/32003[u,v,x,y,z];
L = {x^2, x*y, x*z, y^2, y*z}
C = chainComplex(D, Labels => L)
assert(C.dd^2 == 0)
l = length C
assert((for i to l list rank C_i) == for i to l list (fVector D)#i)
H = homology C
assert(H_0 == S^1/ideal L)
assert all(1..l, i -> H_i == 0)
///


------------------------------------------------------------------------------
-- testing some chain complexes
TEST ///
R = ZZ/101[a..e];
D = simplicialComplex monomialIdeal product gens R
C = chainComplex D
assert(C.dd^2 == 0)
assert(boundaryMap(5,D) == 0)
H = prune HH(C)
assert(rank H_3 == 1)
assert(H_2 == 0)
assert(H_1 == 0)
///

------------------------------------------------------------------------------
TEST ///
R = ZZ/101[symbol x_0 .. symbol x_3]
D = simplicialComplex {x_0 * x_1 * x_2, x_1 * x_2 * x_3}
facets D
dual D
faces(0,D)
chainComplex D
dual D
///

------------------------------------------------------------------------------
-- link of a face
TEST ///
R = ZZ[a..e]
D = simplicialComplex {b*c,c*a,a*e,a*d,c*d,d*e}
I = ideal D
assert(link(D,a) === simplicialComplex{c,d,e})

D = simplicialComplex {b*c,c*a,a*e,a*d,c*d,d*e,a*c*d,a*d*e}
assert(link(D,a) === simplicialComplex{c*d,d*e})
assert(link(D,a*d) === simplicialComplex{c,e})
assert(link(D,c*d) === simplicialComplex{a})
///


------------------------------------------------------------------------------
-- Buchberger/Lyubeznik/Scarf/Taylor complexes of a monomial ideal
TEST ///
S=ZZ/32003[x,y,z];
L1={x^3,x*y,x*z,y^2,y*z,z^2}
R = ZZ/32003[a..f]
D = buchbergerSimplicialComplex(L1,R)
-- peek D.cache.labels
boundaryMap(0,D,Labels=>L1)
boundaryMap(1,D,Labels=>L1)
C = chainComplex(D,Labels=>L1)
assert(C.dd^2 == 0)
prune(HH C)
assert all(0..dim D, i -> HH_(i+1)(C) == 0)
assert(HH_0(C) == S^1/(ideal L1))
assert isHomogeneous C
E = lyubeznikSimplicialComplex(L1,R)
B = chainComplex(E,Labels=>L1)
assert(B.dd^2 == 0)
assert all(0..dim E, i -> HH_(i+1)(B) == 0)
assert(HH_0(B) == S^1/(ideal L1))
assert isHomogeneous B

L2 = {S_0^2,S_1^3,S_0*S_1}
M2 = monomialIdeal L2
LC2A = lyubeznikSimplicialComplex(L2,R)
LR2A = lyubeznikResolution(M2, MonomialOrder => {1,2,0})
assert((prune HH LR2A)_0 == coker gens M2)
assert all(1..length LR2A, i -> (prune HH LR2A)_i == 0)
assert(fVector LC2A === {1,3,3,1})
assert((for i to length LR2A list rank LR2A_i) ==  {1,3,3,1})

LC2B = lyubeznikSimplicialComplex(M2,R)
LR2B = lyubeznikResolution(L2_{2,0,1})
assert((prune HH LR2B)_0 == coker gens M2)
assert all(1..length LR2B, i -> (prune HH LR2B)_i == 0)
assert(fVector LC2B === {1,3,2})
assert((for i to length LR2B list rank LR2B_i) == {1,3,2})

SSC2 = scarfSimplicialComplex(M2,R)
SCC2 = scarfChainComplex(L2)
assert(fVector SSC2 === {1,3,2})
assert((for i to length SCC2 list rank SCC2_i) == {1,3,2})

L3 = {S_0*S_1,S_1*S_2,S_0*S_2}
M3 = monomialIdeal L3
lyubeznikSimplicialComplex(L3,R,MonomialOrder=>{2,1,0})
LR3 = lyubeznikResolution(M3)
assert((prune HH LR3)_0 == coker gens M3)
assert all(1..length LR3, i -> (prune HH LR3)_i == 0)
SSC3 = scarfSimplicialComplex(L3,R)
SCC3 = scarfChainComplex(M3)
assert(SCC3_2 == 0 and dim SSC3 == 0)

M = monomialIdeal(S_0^2,S_0*S_1^2,S_0*S_1*S_2)
T = taylorResolution(M)
T' = taylorResolution(first entries mingens M)
assert(all(1..length T, i -> T.dd_i == T'.dd_i))
M' = monomialIdeal(for i to 5 list S_0^i*S_1^(5-i))
T'' = taylorResolution M'
assert(all(0..length T'', i -> rank T''_i == binomial(numgens M',i)))

--degenerate cases
assert(facets lyubeznikSimplicialComplex(monomialIdeal(0_S),R) ==
    facets simplexComplex(-1,R))
assert(dim lyubeznikSimplicialComplex(monomialIdeal(1_S),R) ==
    dim simplexComplex(0,R))
assert((lyubeznikResolution(monomialIdeal(0_S)))_0 == S^1 and
    (lyubeznikResolution(monomialIdeal(0_S)))_1 == 0)
assert(all({0,1}, i -> (prune HH lyubeznikResolution(monomialIdeal(1_S)))_i == 0))
assert((scarfSimplicialComplex(monomialIdeal(0_S),R))_0 == (coefficientRing R)^1)
assert(facets scarfSimplicialComplex(monomialIdeal(1_S),R) ==
    facets lyubeznikSimplicialComplex(monomialIdeal(1_S),R))
assert((scarfChainComplex(monomialIdeal(0_S)))_0 == S^1 and
    (scarfChainComplex(monomialIdeal(0_S)))_1 == 0)
assert all(0..1,i -> rank (scarfChainComplex(monomialIdeal(1_S)))_1 == 1)
M = monomialIdeal(0_S)
assert((taylorResolution M)_0 == (homology(taylorResolution M))_0)
assert(1_(ring taylorResolution{0_S}) == 1_S)
///


------------------------------------------------------------------------------
-- A generic monomial ideal (Buchberger complex supports the minimal resolution)
TEST ///
S=ZZ/32003[x,y,z]
L={y*z,x^2*z^2,x^2*y^2}
R = ZZ/32003[a..c]
D = buchbergerSimplicialComplex(L,R)
C = chainComplex(D,Labels=>L)
assert(C.dd^2 == 0)
betti C
prune(HH C)
///


------------------------------------------------------------------------------
TEST ///
-- This had been a bug around 0.9.95...
x = getSymbol "x"
S = QQ[x_1..x_5];
Delta = simplicialComplex {x_1*x_2*x_3, x_2*x_4, x_3*x_4, x_5};
C = chainComplex Delta
C.dd
assert(C.dd_0 * C.dd_1 == 0)
assert(C.dd_1 * C.dd_2 == 0)
///


------------------------------------------------------------------------------
-- tests added by Janko
TEST ///
R = QQ[x_0..x_4]
D1 = simplicialComplex monomialIdeal(x_0*x_1*x_2*x_3*x_4)
S = QQ[x_0..x_4,T]
D2 = simplicialComplex monomialIdeal(x_0*x_1*x_2*x_3*x_4,T)
assert(substitute(D1,S) === D2)
///

------------------------------------------------------------------------------
TEST ///
R = QQ[x_0..x_4]
D1 = simplicialComplex monomialIdeal(x_0*x_1*x_2*x_3*x_4)
-- F1=(faces(1,D1,useFaceClass=>true))#0
S = QQ[x_0..x_4,T]
D2 = simplicialComplex monomialIdeal(x_0*x_1*x_2*x_3*x_4,T)
-- F2=(faces(1,D2,useFaceClass=>true))#0
-- assert(substitute(F1,S)==F2)
///


------------------------------------------------------------------------------
TEST ///
R = QQ[x_0..x_4]
D = simplicialComplex monomialIdeal(x_0*x_1*x_2*x_3*x_4)
-- F=(faces(1,D,useFaceClass=>true))#0
-- assert(isFaceOf(F,D))
///


------------------------------------------------------------------------------
TEST ///
R = QQ[x_0..x_4]
D = simplicialComplex monomialIdeal(x_0*x_1,x_2*x_3*x_4)
-- fc=faces(D,useFaceClass=>true)
-- assert(apply(-1..2,j->#fc#j)==(1,5,9,6))
///


------------------------------------------------------------------------------
///
R = QQ[x_0..x_4]
F = face (x_0*x_1)
G = face (x_0*x_1*x_2)
assert(isSubface(F,G))
assert(dim(F)==1);
assert(dim(G)==2);
assert(ring(F)===R)
///


------------------------------------------------------------------------------
///
R = QQ[x_0..x_4]
F = face (x_0*x_1)
assert(set vertices(F) === set {x_0,x_1})
///


------------------------------------------------------------------------------
///
R = QQ[x_0..x_4]
F = face (x_0*x_1)
assert(set vertices(F) === set {x_0,x_1})
///


------------------------------------------------------------------------------
TEST ///
R = QQ[a..e]
D = simplicialComplex monomialIdeal(a*b*c*d*e)
-- assert(D==simplicialComplex facets(D,useFaceClass =>true))
///
------------------------------------------------------------------------------
--Testing different labellings of simplicial complexes
TEST///
A = QQ[x_0..x_4];
D = simplicialComplex{A_0*A_2*A_3,A_2*A_3*A_4}
vertices D
S = QQ[x_0..x_3]
-- This complex should be a minimal free resolution of ideal {S_0*S_1,S_3,S_1*S_2,S_0*S_2}
C = chainComplex(D,Labels => {S_0*S_1,S_3,S_1*S_2,S_0*S_2})
assert((for i to length C list rank C_i) === fVector D)
assert all(1..length C, i -> ((homology C)_i == 0))
-- This complex is not a minimal free resolution. It is not even exact.
C = chainComplex(D,Labels => {S_3,S_0*S_1,S_1*S_2,S_0*S_2})
assert(not (homology C)_1 == 0)
///


------------------------------------------------------------------------------
-- Tests for simplicial maps
TEST ///
S = QQ[w,x,y,z];
D = simplexComplex(3, S)
R = QQ[s,t];
E = simplexComplex(1,R)
f = map(E, D, matrix {{s,t,t,s}})
assert isWellDefined f
h = map(E, D, {s,t,t,s})
assert (h === f)
assert isSurjective f
g = map(D,E,{x,y})
assert isWellDefined g
assert isInjective g
assert (fVector image g === fVector E)
assert (ring image g === ring D)
///


------------------------------------------------------------------------------
-- Testing chainComplex of a simplicial map
TEST ///
R = QQ[a,b,c,d,e,f]
D = simplicialComplex({a*b*c, b*c*d, d*e*f})
D' = simplicialComplex({a*b*c, c*d, d*e*f})
phi = map(D, D', {a,b,c,d,e,f})
assert isWellDefined phi
Phi = chainComplex phi
assert (Phi * (source Phi).dd == (target Phi).dd * Phi)
assert ((source Phi) === (chainComplex D'))
assert ((target Phi) === (chainComplex D))
///


------------------------------------------------------------------------------
-- Testing (homology, SimplicialMap)
S = ZZ/101[a,b,c,d]
D = simplicialComplex{1_S}
E = simplicialComplex{0_S}
f = map(D,E,map(S,S))
assert((homology f)_(-1) == 0)
assert((homology f)_0 == 0)
g = map(simplexComplex(2,S), D, map(S,S))
assert all(-1..2, i -> (homology g)_i == 0)
D = simplicialComplex{a*b, b*c, a*c}
I = map(D,D, id_(ring D))
assert((homology I)_(-1) == 0)
assert((homology I)_0 == 0)
assert((prune homology I)_1 == matrix{{1_(coefficientRing D)}})

------------------------------------------------------------------------------
-- Testing more chainComplex maps
-- These examples come from Munkres' Algebraic Topology
-- Example 1 of Ch. 1, Sec. 12, page 63-64.
TEST ///
R = ZZ[x_0..x_10]
Torus = simplicialComplex{
    R_0*R_3*R_4, R_0*R_1*R_4, R_1*R_2*R_4, R_2*R_4*R_5,
    R_0*R_2*R_5, R_0*R_3*R_5, R_3*R_4*R_6, R_4*R_6*R_7,
    R_4*R_7*R_9, R_4*R_5*R_9, R_7*R_8*R_9, R_5*R_8*R_9,
    R_3*R_5*R_8, R_3*R_6*R_8, R_0*R_6*R_7, R_0*R_1*R_7,
    R_1*R_7*R_8, R_1*R_2*R_8, R_0*R_2*R_8, R_0*R_6*R_8
    }
S = ZZ[y_0..y_5]
Circle = simplicialComplex(for i to 5 list S_i*S_((i+1)%6))
f = map(Torus,Circle,matrix{{R_0,R_4,R_3,R_3,R_4,R_6}})
Cf = chainComplex f
CCircle = source Cf
CTorus = target Cf
assert not isInjective f
assert all(1, i -> Cf_(i-1)*CCircle.dd_i == CTorus.dd_i*Cf_i )
g = map(Torus,Circle,matrix{{R_0,R_1,R_2,R_0,R_4,R_3}})
Cg = chainComplex g
assert all(1, i -> Cg_(i-1)*CCircle.dd_i == CTorus.dd_i*Cg_i)
h = map(Torus,Circle,matrix{{R_0,R_7,R_8,R_5,R_5,R_0}})
Ch = chainComplex h
assert all(1, i -> Ch_(i-1)*CCircle.dd_i == CTorus.dd_i*Ch_i)
///


------------------------------------------------------------------------------
-- Testing relative homology of simplicial complexes.
TEST ///
S = ZZ[y_0..y_5]
Circle = simplicialComplex(for i to 5 list y_i*y_((i+1)%6))
Irrelevant = simplicialComplex{1_S}
OnePoint = simplicialComplex{S_0}
TwoPoints = simplicialComplex{S_0, S_2}

H = prune homology(Circle, Irrelevant)
H' = prune homology(Circle, OnePoint)
H'' = prune homology(Circle, TwoPoints)

assert (rank H_0 == 1)
assert (rank H'_0 == 0)
assert (rank H'_1 == 1)
assert (rank H''_1 == 2)
///


------------------------------------------------------------------------------
-- Testing barycentricSubdivision
TEST ///
R = ZZ/101[x_0..x_2]
T = ZZ/101[y_0..y_6]
S = ZZ/101[z_0..z_25]
D = simplicialComplex{x_0*x_1*x_2}
E = barycentricSubdivision(D,T)
assert (#vertices E == sum (for i to 2 list #((faces D)#i)))
f = map(E,D,{y_2,y_5,y_6})
assert isWellDefined f
assert isInjective f
g = barycentricSubdivision(id_D, T, T)
assert isWellDefined g
h = barycentricSubdivision(f,S,T)
assert isWellDefined h
irrelevant = simplicialComplex {1_R}
bIrrelevant = barycentricSubdivision(irrelevant, R)
irrelevant === bIrrelevant
///


------------------------------------------------------------------------------
-- Testing elementaryCollapse
TEST ///
R = ZZ[a..d]
triangle = simplicialComplex {a*b*c}
line = elementaryCollapse(triangle, a*b)
assert (line === simplicialComplex {a*c, b*c})
///


------------------------------------------------------------------------------
-- Testing wedge
TEST ///
R = ZZ[x_0..x_4]
S = ZZ[y_0..y_4]
D = simplicialComplex {R_0*R_1*R_2,R_0*R_2*R_3*R_4,R_0*R_4}
E = simplicialComplex {S_0*S_1*S_2*S_3*S_4}
W = wedge(D, E, R_4, S_4)
assert isWellDefined W
assert (#vertices W == #vertices D + #vertices E - 1)
///


------------------------------------------------------------------------------
-- Testing prune
TEST ///
R = ZZ[x_0..x_20]
D = simplicialComplex {R_0*R_1*R_2}
D' = prune D
assert (numgens ring D' == 3)
///

------------------------------------------------------------------------------
-- Testing Connected Components

-- For the void and empty complex, we should return that complex
R = QQ[x_0 .. x_11]
void = simplicialComplex monomialIdeal(1_R)
assert(#(connectedComponents void) == 1)
assert((connectedComponents void)#0 === void)


empty = simplicialComplex {1_R}
assert(#(connectedComponents empty) == 1)
assert((connectedComponents empty)#0 === empty)

D = simplicialComplex {x_0*x_1, x_1*x_2, x_3*x_4, x_4*x_5*x_6, x_6*x_7, x_8*x_9*x_10, x_9*x_10*x_11}
assert(#(connectedComponents D) == 3)
P = positions(connectedComponents D, D -> D === simplicialComplex{x_6*x_7, x_4*x_5*x_6, x_3*x_4})
assert(#P == 1)


-*
-- For later use?
KleinBottle = simplicialComplex{
    R_0*R_3*R_4, R_0*R_1*R_4, R_1*R_2*R_4, R_2*R_4*R_5,
    R_0*R_2*R_5, R_0*R_5*R_6, R_3*R_4*R_6, R_4*R_6*R_7,
    R_4*R_7*R_9, R_4*R_5*R_9, R_7*R_8*R_9, R_5*R_8*R_9,
    R_5*R_6*R_8, R_3*R_6*R_8, R_0*R_6*R_7, R_0*R_1*R_7,
    R_1*R_7*R_8, R_1*R_2*R_8, R_0*R_2*R_8, R_0*R_3*R_8
    }

RealProjectivePlane = simplicialComplex{
    R_0*R_4*R_5,  R_0*R_1*R_5,  R_1*R_2*R_5,  R_2*R_5*R_6,
    R_2*R_3*R_6,  R_3*R_6*R_7,  R_4*R_5*R_7,  R_5*R_7*R_8,
    R_5*R_8*R_10, R_5*R_6*R_10, R_8*R_9*R_10, R_6*R_9*R_10,
    R_6*R_7*R_9,  R_4*R_7*R_9,  R_3*R_7*R_8,  R_2*R_3*R_8,
    R_2*R_8*R_9,  R_1*R_2*R_9,  R_0*R_1*R_9,  R_0*R_4*R_9
    }

MobiusStrip = simplicialComplex{
    R_0*R_3*R_4,  R_0*R_1*R_5,
    R_1*R_2*R_5,  R_2*R_5*R_6,
    R_2*R_3*R_6,  R_0*R_3*R_6
    }
*-
