-- Here are some examples and tests of our own.

compactMatrixForm = false
path = prepend("../", path)
needsPackage "Schubert2"
F = flagBundle ( {4,2}, VariableNames => {,c} )
AF = intersectionRing F
schubertCycle'((0,1),F)
F_(0,1)
integral oo
assert( oo == 1 )
F_(4,5)
assert( oo == 1 )
t = {1, c_1, c_2, c_1^2-c_2, c_1*c_2, c_2^2, c_1^3-2*c_1*c_2, c_1^2*c_2-c_2^2, c_1*c_2^2, c_2^3, c_1^4-3*c_1^2*c_2+c_2^2, c_1^3*c_2-2*c_1*c_2^2, c_1^2*c_2^2-c_2^3, c_1*c_2^3, c_2^4}
b = reverse flatten for i from 0 to 5 list for j from i+1 to 5 list F_(i,j)
assert( b == t )
d = flatten for i from 0 to 4 list for j from 0 to i list F_{i,j}
assert( d == t )

clearAll

W = abstractProjectiveSpace 7;
h = chern_1(OO_W(1));
assert(integral(h^7) == 1)
E = 4*OO_W ++ OO_W(-1);
L = OO_W(1);
classY = skewDegeneracyLocus2(2,E,L);
assert(integral(h^4*classY) == 13)
Y = skewDegeneracyLocus(2,E,L);							     
assert(chi(tangentBundle Y) == -80)

clearAll

W = weightedProjectiveSpace {1,1,1,1,1,1,2,2}
h = chern_1(OO_W(1));
assert(integral(h^7) == 1/4)
E = 3*OO_W ++ 2*OO_W(-1);
L = OO_W(1);
classY = skewDegeneracyLocus2(2,E,L);
assert(integral(h^4*classY) == 7)
Y = skewDegeneracyLocus(2,E,L);							     
assert(chi(tangentBundle Y) == -94)
-- Y is a smooth 4-fold that avoids the points of W with nontrivial stabilizer group,
-- and so "chi" gives meaningful answers on Y.

clearAll

W = abstractProjectiveSpace 9;
h = chern_1(OO_W(1));
E = 5*OO_W;
L = OO_W(1);
classY = symmetricDegeneracyLocus2(2,E,L);
assert(integral(h^3*classY) == 35)
Y = symmetricDegeneracyLocus(2,E,L);							     
assert(chern_1(tangentBundle Y) == 0)
-- That is, Y is a Calabi-Yau 3-fold of degree 35 in P^9.

clearAll

