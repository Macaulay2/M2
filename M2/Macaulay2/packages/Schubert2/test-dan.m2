P3 = flagBundle{3,1}
(R,Q) = P3.Bundles
p = P3.StructureMap
A = intersectionRing P3
chern_1 R
chern_1 Q
chern_2 R
chern_2 Q
p_* chern_3 Q
p^* 11
transpose presentation A
basis A

G24 = flagBundle{2,2}
(R,Q) = G24.Bundles
C = intersectionRing G24
transpose presentation C

F22 = flagBundle{2,2}
(R,Q) = F22.Bundles
A = intersectionRing F22
transpose presentation A
basis A

clearAll
A = QQ[e_1 .. e_4,Degrees=>{1,2,3,4}]
B = A/(e_1^5,e_2^3,e_3^2,e_4^2)
X = abstractVariety(4,B)
E = abstractSheaf(X,Rank => 4,ChernClass => 1 + sum(1 .. 4, i -> e_i))
F22 = flagBundle({2,2},E)
(R,Q) = F22.Bundles
p = F22.StructureMap
C = intersectionRing F22
(chern_2 R)
(chern_2 R)^2
(chern_1 Q)^8
p_* (chern_1 Q)^8

F222 = flagBundle({2,2,2})
(P,R,S) = F222.Bundles
p = F222.StructureMap
dim p
B = intersectionRing F222
transpose presentation B
transpose basis B
(chern_1 P)^3 * (chern_1 R)^5 * (chern_1 S)^4
p_* oo

base(3, Bundle => (symbol E,3,symbol e))
P = flagBundle({2,1}, E)
(W,Q) = P.Bundles
C = intersectionRing P
ch Q
netList toList parts ch Q

gens C
degree \ gens C
describe C
chern_1 Q
chern_1 W
chern_1 W + chern_1 Q
assert( chern_1 W + chern_1 Q == e_1 )
(chern_2 W)^2
rank Q
chern_1 det Q
promote(e_1,C)
dim p
parts ch Q
assert( value parts ch Q == ch Q )

p = P.StructureMap
p_* (chern_1 W)^2

clearAll

X = base(5, Bundle => (L,1,{l}), Bundle => (M,1,{m}), Bundle => (N,1,{n}))
assert( degeneracyLocus2(0,L+M+N,OO_X) == l*m*n )
assert( degeneracyLocus2(0,OO_X,L+M+N) == -l*m*n )
assert( degeneracyLocus2(1,L+M+N,2*OO_X) == l*m + l*n + m*n )
assert( degeneracyLocus2(2,L+M+N,3*OO_X) == l + m + n )

clearAll

-- from arxiv:9906119 by Tjotta
V = OO_point^7
P = projectiveBundle'( exteriorPower_2 V, VariableNames => {,{p}} )
PG = flagBundle( {4,3}, (P/point)^* V, VariableNames => {beta,gamma} )
(U,Q) = PG.Bundles
A = exteriorPower_2 ( (PG/point)^* V  )  *  OO(p) - exteriorPower_2 U * OO(p)
M = sectionZeroLocus A
B = intersectionRing M
p = promote(p,B)
g2 = promote(gamma_2,B)
integral ( p^17 )
assert( oo == 14 )
integral ( g2 * p^15 )
assert( oo == 28 )
integral ( g2^2 * p^13 )
assert( oo == 59 )
integral ( g2^3 * p^11 )
assert( oo == 117 )

F = flagBundle_{2}(OO_point^10,Isotropic=>true)
Q = first F.Bundles
assert ( 1430 == integral (chern_1 Q)^15)

F = flagBundle_{5}(OO_point^10,Isotropic=>true)
Q = first F.Bundles
assert ( 292864 == integral (chern_1 Q)^15)
