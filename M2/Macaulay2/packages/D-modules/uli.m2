makeWA = method()
makeWA (PolynomialRing) := (R) -> (
     makeWeylAlgebra R)

makeWeylAlgebra = method()
makeWeylAlgebra (PolynomialRing) := (R) -> (
     coordVars := (entries vars R)#0;
     diffVars := apply(coordVars, i -> (value("d" | toString(i))) );
     allVars := join(coordVars, diffVars);
     W := (coefficientRing R)[allVars, WeylAlgebra =>
          apply(toList(0..#coordVars-1), i -> (coordVars_i=>diffVars_i) )];
     W)
