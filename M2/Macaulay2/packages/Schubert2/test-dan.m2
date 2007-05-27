loadPackage "Schubert2"
compactMatrixForm = false

P3 = flagBundle({3,1},BundleNames => {R,Q})
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

G24 = flagBundle({2,2},BundleNames=>{R,Q})
C = intersectionRing G24
transpose presentation C

F22 = flagBundle({2,2},BundleNames=>{R,Q})
A = intersectionRing F22
transpose presentation A
basis A

clearAll
A = QQ[e_1 .. e_4,Degrees=>{1,2,3,4}]
B = A/(e_1^5,e_2^3,e_3^2,e_4^2)
X = abstractVariety(4,B)
E = abstractSheaf(X,Rank => 4,ChernClass => 1 + sum(1 .. 4, i -> e_i))
F22 = flagBundle({2,2},E,BundleNames=>{R,Q})
p = F22.StructureMap
C = intersectionRing F22
(chern_2 R)
(chern_2 R)^2
(chern_1 Q)^8
p_* (chern_1 Q)^8

F222 = flagBundle({2,2,2},BundleNames=>{P,R,S})
p = F222.StructureMap
dim p
B = intersectionRing F222
transpose presentation B
transpose basis B
(chern_1 P)^3 * (chern_1 R)^5 * (chern_1 S)^4
p_* oo

E = bundle(3, 3, e)
P = flagBundle(E,BundleNames=>{W,Q})
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
