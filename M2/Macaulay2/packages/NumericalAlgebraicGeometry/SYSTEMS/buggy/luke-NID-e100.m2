restart
needsPackage("Bertini")
R = CC[x,y,z];
f = {(1e100*y^2+x^2+z^2-1)*x, (y^2+x^2+z^2-1)*y};

NV = bertiniPosDimSolve f

f = {(10^100*y^2+x^2+z^2-1)*x, (y^2+x^2+z^2-1)*y};
NV = bertiniPosDimSolve f

restart
needsPackage("NumericalAlgebraicGeometry")
R = CC[x,y,z];
f = {(1e100*y^2+x^2+z^2-1)*x, (y^2+x^2+z^2-1)*y};

numericalIrreducibleDecomposition ideal f -- empty set (incorrect)
numericalIrreducibleDecomposition (ideal f, Software=>BERTINI) -- fails... does it get an empty set and just fails to parse files in this scenario? 
numericalIrreducibleDecomposition (ideal f, Software=>PHCPACK) -- nonempty set (still incorrect)

restart
needsPackage("NumericalAlgebraicGeometry")
R = CC[x,y,z];
f = {(1e1*y^2+x^2+z^2-1)*x, (y^2+x^2+z^2-1)*y};

numericalIrreducibleDecomposition (ideal f, Software=>BERTINI) -- looks OK
numericalIrreducibleDecomposition (ideal f, Software=>PHCPACK) -- does PHC return junk components too?
numericalIrreducibleDecomposition ideal f -- this gets stuck? (NID in M2 is going to be reimplemented)

