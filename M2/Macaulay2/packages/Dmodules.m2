-- -*- coding: utf-8 -*-
newPackage("Dmodules", 
     Version => "1.1",
     Date => "10/01/2008",
     Headline => "functions for computations with D-modules",
     HomePage => "http://www.math.uic.edu/~leykin/Dmodules",
     AuxiliaryFiles => true,
     Authors => {
	  {Name => "Anton Leykin", Email => "leykin@math.uic.edu"},
	  {Name => "Harrison Tsai"}
	  }
     )

load "./Dmodules/Dmodules.m2"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=Dmodules pre-install"
-- End:
