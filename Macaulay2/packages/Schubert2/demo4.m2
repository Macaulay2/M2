G = flagBundle({2,2}, VariableNames => {b,c})
G.StructureMap
target G.StructureMap
intersectionRing oo
intersectionRing G
G.Bundles
(S,Q) = G.Bundles
G.BundleRanks
Q
chern Q
chern S
chern (S + Q)
I = ideal intersectionRing G
groebnerBasis I
compactMatrixForm = false
transpose groebnerBasis I
c_1^2
c_1^3
c_1^4
integral oo
-- Lines on hypersurfaces:
B = symmetricPower_3 Q
integral chern B
code(integral, class chern B)
G.StructureMap_*
G.StructureMap_* chern B
code(G.StructureMap_*, class chern B)
chern B
f = n -> (
     -- number of lines in P^n lying on a hypersurface
     -- of degree 2n-3
     G := flagBundle {n-1,2};
     integral chern symmetricPower_(2*n-3) last G.Bundles)
f 3
f 10
-- Conics on a quintic threefold:
G = flagBundle {2,3} -- planes in P^4
(S,Q) = G.Bundles
B = symmetricPower_2 Q -- quadratic forms on them
-- space of conics in planes in P^4:
X = projectiveBundle(dual B, VariableNames => {,{z}})
-- forms of degree 5 on planes in P^4 modulo those vanishing
-- on the conic
A = symmetricPower_5 Q - symmetricPower_3 Q ** OO(-z)
-- the number of conics lying in a quintic hypersurface
integral chern A
-- intersection rings with parameters:
pt = base symbol n
X = projectiveSpace_1 pt
chi OO_X(1)
chi OO_X(2)
chi OO_X(3)
chi OO_X(n)
X = projectiveSpace_2 pt
chi OO_X(n)
tangentBundle X
F = symmetricPower_n oo
chern F
integral oo
ch F
chi F
-- The Horrocks-Mumford bundle:
X = projectiveSpace_4 pt
A = intersectionRing X
cotangentBundle X
exteriorPower_2 oo
F = 2 * (exteriorPower_2 cotangentBundle X)(2) - 5 * OO_X(-1) - 5 * OO_X
chern F
chi F(n*h)
-- base varieties:
S = base(2,p,q,Bundle =>(A,1,a), Bundle => (B,2,{b1,b2}))
A
S = base(2,p,q,Bundle =>(symbol A,1,a), Bundle => (B,2,{b1,b2}))
B
S = base(2,p,q,Bundle =>(symbol A,1,a), Bundle => (symbol B,2,{b1,b2}))
p
A
chern A
B
chern B
chern(A*B)
X = projectiveSpace(3,S,VariableName => H)
intersectionRing X
f = X.StructureMap
-- check the projection formula:
x = chern f_* (f^* OO_S(p*a_1) * OO_X(q*H))
y = chern f_* OO_X((f^*(p*a_1))+q*H)
x == y
-- schubertCycle()
base(2, Bundle=>(A, n=8, a))
F = flagBundle ({5,q=3},A)
CH = intersectionRing F;
describe CH
F_(1,3,5)
{n-q+0-1,n-q+1-3,n-q+2-5}
-- abstractVariety
A = QQ[c,d,Degrees=>{1,2}]
X = abstractVariety(3,A)
peek A
intersectionRing X
F = abstractSheaf(X, ChernCharacter => 3+c+d)
ch F
rank F
chern F
adams_101 F
ch oo
chern ooo
-- Chern class variables:
S = symbol S
Q = symbol Q
F = flagBundle({3,2},VariableNames => {{chern_1 S,chern_2 S,chern_3 S},{chern_1 Q,chern_2 Q}})
A = intersectionRing F
(S,Q) = F.Bundles
chern Q
chern S
