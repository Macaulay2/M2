-- -*- coding: utf-8 -*-
newPackage("Dmodules", 
     Version => "1.4.0.1",
     Date => "01/28/2011",
     Headline => "functions for computations with D-modules",
     HomePage => "http://people.math.gatech.edu/~aleykin3/Dmodules",
     AuxiliaryFiles => true,
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.gatech.edu"},
	  {Name => "Harrison Tsai"}
	  },
     DebuggingMode => false,
     PackageImports => {"PrimaryDecomposition","ReesAlgebra","Elimination"}
     )

load "./Dmodules/Dmodules.m2"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Dmodules pre-install"
-- End:
