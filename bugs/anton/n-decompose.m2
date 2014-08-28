restart
needsPackage "NumericalAlgebraicGeometry"
setRandomSeed 7
R = CC[x,y]
F = {x^2+y^2-1, x*y};
W = first regeneration F 
decompose W

-- reduced
R = CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
I = ideal {sph*(x-1)*(y-x^2), sph*(y-2)*(z-x^3)};
reg = regeneration I_* 
decompose reg#0

-- non-reduced (one component with multiplicity)
R = QQ[x,y,z]
R = CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
I = ideal {sph*(x-1)*(y-x^2), sph*(y-1)*(z-x^3)};
reg = regeneration I_* 
decompose reg#0

restart
needsPackage "NumericalAlgebraicGeometry"
R = CC[x,y,z]
sph = (x^2+y^2+z^2-1); 
I = ideal {sph*(x-1)*(y-x^2), sph*(y-1)*(z-x^3)};
setRandomSeed 7
V = numericalIrreducibleDecomposition I 
peek V
