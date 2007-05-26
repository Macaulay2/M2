loadPackage "Schubert2"
compactMatrixForm = false

(P3,p) = Proj(3,{R,Q})
A = intersectionRing P3
c_1 R
c_1 Q
c_2 R
c_2 Q
p_* c_3 Q
p^* 11
transpose presentation A
basis A

(G24,q) = Grassmannian(2,4,{R,Q})
C = intersectionRing G24
transpose presentation C

(F22,r) = flagBundle(4,{R,Q},{2,2})
A = intersectionRing F22
transpose presentation A
basis A

A = QQ[e_1 .. e_4,Degrees=>{1,2,3,4}]
B = A/(e_1^5,e_2^3,e_3^2,e_4^2)
X = abstractVariety(4,B)
E = abstractSheaf(X,Rank => 4,ChernClass => 1 + sum(1 .. 4, i -> e_i))
(F22,p) = flagBundle(E,{R,Q},{2,2})
C = intersectionRing F22
(c_2 R)
(c_2 R)^2
(c_1 Q)^8
p_* (c_1 Q)^8

(F222,p) = flagBundle(6,{P,R,S},{2,2,2})
dim p
B = intersectionRing F222
transpose presentation B
transpose basis B
(c_1 P)^3 * (c_1 R)^5 * (c_1 S)^4
p_* oo

E = bundle(3, 3, e)
(P,p) = Proj(E,{W,Q})
C = intersectionRing P
ch Q
netList toList parts ch Q

gens C
degree \ gens C
describe C
c_1 Q
c_1 symbol W
c_1 W
assert( c_1 symbol W == c_1 W )
c_1 W + c_1 Q
assert( c_1 W + c_1 Q == e_1 )
(c_2 W)^2
rank Q
c_1 det Q
promote(e_1,C)
dim p
parts ch Q
assert( value parts ch Q == ch Q )

p_* (c_1 W)^2

-- parameters
(P3,p) = Proj(3,{R,Q});
AP3 = intersectionRing P3;
factor chi OO_P3(n)
todd P3
