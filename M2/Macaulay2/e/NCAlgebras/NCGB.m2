restart
needsPackage "NCAlgebra"
Q = (ZZ/32003){x,y,z}
Q = QQ{x,y,z}
phi = ncMap(Q,Q,{y,z,x})
f = 2*x*y + 3*y*x + 5*z^2
f = x*y + y*x - 2*z^2
I = ncIdeal {f, phi f, phi phi f}
elapsedTime (Igb = ncGroebnerBasis(I, DegreeLimit => 15));
{-2*z^2+y*x+x*y, z*y+y*z-2*x^2, z*x-2*y^2+x*z}
{y^2*x-x*y^2, y*x^2-x^2*y, z*x-2*y^2+x*z, z*y+y*z-2*x^2, z^2-1/2*y*x-1/2*x*y}

restart
needsPackage "AssociativeAlgebras"
gbTrace = 4
kk = QQ
R = kk{z,y,x}

I = ideal(-2*z^2+y*x+x*y, z*y+y*z-2*x^2, z*x-2*y^2+x*z)
NCGB(I, 10)

I = ideal(5*z^2+3*y*x+2*x*y, 3*z*y+2*y*z+5*x^2, 2*z*x+5*y^2+3*x*z)
elapsedTime NCGB(I, 10);
J = elapsedTime NCGB(I, 15);
elapsedTime NCGB(I, 20);

kk = ZZ/32003
R = kk{z,y,x}

B = flatten for i from 0 to 2 list flatten for j from 0 to 2 list for k from 0 to 2 list R_i * R_j * R_k
inJ = (ideal J)_*/leadTerm//ideal
compress NCReduction2Sided(matrix{B}, inJ)

kk = ZZ/32003
R = kk{x,y}
I = ideal(x^2-y^2)
J = elapsedTime NCGB(I, 10);
B = flatten for i from 0 to 1 list flatten for j from 0 to 1 list for k from 0 to 1 list R_i * R_j * R_k
inJ = (ideal J)_*/leadTerm//ideal
compress NCReduction2Sided(matrix{B}, inJ)

restart
needsPackage "AssociativeAlgebras"
kk = ZZ/32003
R = kk{z,y,x}
I = ideal(5*z^2+3*y*x+2*x*y, 3*z*y+2*y*z+5*x^2, 2*z*x+5*y^2+3*x*z)
gbTrace = 4
J = elapsedTime NCGB(I, 9);
-- 36 seconds with WordTable
-- 44.5 seconds with SuffixTree :(
-- no doubt due to too many copies in the suffix tree code.
-- will optimize next by using more pointers and less full copies.
J = elapsedTime NCGB(I, 20);
B = flatten for i from 0 to 2 list flatten for j from 0 to 2 list for k from 0 to 2 list R_i * R_j * R_k
inJ = (ideal J)_*/leadTerm//ideal
compress NCReduction2Sided(matrix{{B_13}}, inJ) -- crashes
compress NCReduction2Sided(matrix{{B_15}}, inJ) -- crashes

compress NCReduction2Sided(matrix{B}, inJ)
compress NCReduction2Sided(matrix{{y^3}}, J)
compress NCReduction2Sided(matrix{{(y*x^2)^2}}, J)

compress NCReduction2Sided(matrix{{B_0}}, inJ)
compress NCReduction2Sided(matrix{{B_1}}, inJ)
for i from 0 to 12 list elapsedTime compress NCReduction2Sided(matrix{{B_i}}, inJ)

compress NCReduction2Sided(matrix{{y^3}}, ideal(z^2, z*y, z*x, y^2*x, y^2*z, y*x*y^2))
compress NCReduction2Sided(matrix{{y^3}}, ideal(z^2, z*y, z*x))
compress NCReduction2Sided(matrix{{y^3}}, ideal(z^2, z*y, z*x, y^2*x, y^2*z))
compress NCReduction2Sided(matrix{{y^3}}, ideal(y*x*y^2)) -- crash


restart
needsPackage "AssociativeAlgebras"

kk = ZZ/32003
R = kk{x,y}
compress NCReduction2Sided(matrix{{y}}, ideal(y^2)) -- crash


restart
needsPackage "AssociativeAlgebras"
kk = frac(QQ[a,b,c])
R = kk{x,y,z}
I = ideal(a*x*y + b*y*x + c*z^2, a*y*z + b*z*y + c*x^2, a*z*x + b*x*z + c*y^2)
elapsedTime Igb = NCGB(I,5)
elapsedTime Igb = NCGB(I,6)

restart
needsPackage "AssociativeAlgebras"
R = QQ{x,y}
I = ideal{x^2 - y^2}
Igb = NCGB(I,3)
