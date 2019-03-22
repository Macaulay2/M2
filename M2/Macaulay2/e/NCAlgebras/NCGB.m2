restart
needsPackage "NCAlgebra"
Q = (ZZ/32003){x,y,z}
Q = QQ{x,y,z}
phi = ncMap(Q,Q,{y,z,x})
f = 2*x*y + 3*y*x + 5*z^2
f = x*y + y*x - 2*z^2
I = ncIdeal {f, phi f, phi phi f}
elapsedTime (Igb = ncGroebnerBasis(I, DegreeLimit => 10))
{-2*z^2+y*x+x*y, z*y+y*z-2*x^2, z*x-2*y^2+x*z}
{y^2*x-x*y^2, y*x^2-x^2*y, z*x-2*y^2+x*z, z*y+y*z-2*x^2, z^2-1/2*y*x-1/2*x*y}

restart
needsPackage "PolynomialAlgebra"
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
NCReduction2Sided(matrix{B}, inJ)

restart
needsPackage "PolynomialAlgebra"
kk = frac(QQ[a,b,c])
R = kk{x,y,z}
I = ideal(a*x*y + b*y*x + c*z^2, a*y*z + b*z*y + c*x^2, a*z*x + b*x*z + c*y^2)
elapsedTime Igb = NCGB(I,6)

