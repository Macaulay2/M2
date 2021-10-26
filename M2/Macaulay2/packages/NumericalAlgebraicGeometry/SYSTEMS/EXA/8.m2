needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
I = ideal {sph*(x-1)*(y-x^2), sph*(y-2)*(z-x^3)};
V := numericalIrreducibleDecomposition I 
peek V
