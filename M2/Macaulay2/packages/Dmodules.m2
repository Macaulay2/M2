newPackage( Dmodules, Version => "1.0")
needs "Dmodules/Dloadfile.m2"
needs "Dmodules/DMODdoc.m2"
-- erase the internal symbols
scan({"createCommAlgebra", "CommAlgebra", "createAssCommRing", "ThetaRing",
     "createThetaRing", "createIntRing", "createHomWeylAlgebra", "zeroize",
     "DBIGPRIME", "computeLocalization", "invPermute", "gbW1",
     "gbW2", "inW1", "inW2", "WAtoCR", "WAtoCA", "CAtoWA",
     "AnnIFs2", "kerGB", "kerGBstatus", "ResToOrigRing", "CRtoWA",
     "isGeneric", "IRtoR", "CommRing", "RtoIR", "WtoT", "HWAtoWA",
     "WAtoHWA", "HomWeylAlgebra", "RestrictComplex", "BFunction",
     "LocalizeMap", "diagonal", "computeRestriction", "TwistOperator",
     "divideOutGCD"},
  s -> (
       if not Dmodules.Dictionary#?s then error("expected ",s," to be a symbol first defined by the Dmodules package");
       erase Dmodules.Dictionary#s))
closePackage Dmodules
