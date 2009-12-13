G = flagBundle({2,2}, VariableNames => {b,c})
G.StructureMap
target G.StructureMap
intersectionRing oo
intersectionRing G
G.Bundles
(S,Q) = G.Bundles
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
pt = base n
X = projectiveSpace_1 pt
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
