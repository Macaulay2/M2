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