
document {
     Key => {[Dres,Strategy],[Dresolution,Strategy]},
     Headline => "strategy for computing a resolution of a D-module",
     UL { 
	  {BOLD "Schreyer", 
	       " -- uses Schreyer method in homogeneous Weyl algebra"},
	  {BOLD "Vhomogenize", " -- uses V-homogenization method of Oaku"}
     	  }
     }
document {
     Key => {[Dres,LengthLimit],[Dresolution,LengthLimit]},
     Headline => "the limit for the length of a resolution of a D-module",
     "In case the actual length of the resolution exceeds the limit, it is truncated.
     The default value is infinity." 
     }
document {
     Key => Schreyer,
     Headline => "strategy for computing a resolution of a D-module"
     }
document {
     Key => Vhomogenize,
     Headline => "strategy for computing a resolution of a D-module"
     }

document {
     Key => {Dresolution, (Dresolution,Module), (Dresolution,Ideal,List), 
	  (Dresolution,Module,List), (Dresolution,Ideal)},
     Headline => "resolution of a D-module",
     Usage => "Dresolution M, Dresolution I, Dresolution(M,w), Dresolution(I,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  ChainComplex => {"a Schreyer resolution of the D-module ", EM "M", 
	       " or a resolution adapted to a weight vector ", EM "w", 
	       " of the form ", EM "(-u,u)"}
	  },
     "This routine computes various resolutions of a D-module.
     If no weight vector is specified, then the command
     produces a resolution by using the Schreyer order implemented
     in the engine.  If a weight vector ", EM "w", " of the form ", EM "(-u,u)", 
     " is specified, then the command produces a resolution with shifts
     which is adapted to the weight vector ", EM "w", ".  
     These ", EM "w", "-adapted resolutions are compatible
     with b-functions and used in the restriction algorithm.
     For ordinary resolutions, the user may use the command ", TT "resolution", ".
     Note that the notion of a minimal resolution is well-defined only in case 
     of homogenized Weyl algebra.",
     PARA{},
     "There are two strategies for constructing
     w-adapted resolutions.   The first strategy is to construct
     a Schreyer resolution in the homogenized Weyl algebra
     and then dehomogenize.  The second strategy is to homogenize
     with respect to the weight vector.
     These strategies are described in the paper
     'Algorithms for D-modules'
     by Oaku-Takayama(1999).",
     EXAMPLE lines ///
	     R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
	     I = ideal(x_1*D_1+3*x_2*D_2-1, D_1^3-D_2)
	     Dresolution(I,{-1,-1,1,1})
	     ///,
     "Abbreviations :",
     UL{"Dres"},

     SeeAlso => {"gbw", "Drestriction"}
     }

document {
     Key => {[DintegrationIdeal,Strategy],
	  [DintegrationComplex,Strategy],
	  [DintegrationAll,Strategy],[DintegrationClasses,Strategy],
	  [Drestriction,Strategy],[DrestrictionIdeal,Strategy],
	  [DrestrictionComplex,Strategy],[Drestrict,Strategy],
	  [DrestrictionAll,Strategy],[DrestrictionClasses,Strategy]},
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
	  }

document {
     Key => {Drestriction, (Drestriction,ZZ,Module,List), (Drestriction,Ideal,List), 
	  (Drestriction,Module,List), (Drestriction,ZZ,Ideal,List)},
     Headline => "restriction modules of a D-module",
     Usage => "N = Drestriction(M,w), NI = Drestriction(I,w), Ni = Drestriction(i,M,w),
     NIi = Drestriction(i,I,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector",
	  "i" => ZZ => "nonnegative"  
	  },
     Outputs => {
	  "Ni" => Module => {"the i-th derived integration module of ", EM "M"," with respect
     to the weight vector ", EM "w"},
     	  "N" => HashTable => {"contains entries of the form ", TT "i=>Ni"},
	  "NIi" => Module => {"the i-th derived integration module of ", EM "D/I", " with respect
     to the weight vector ", EM "w"},
     	  "NI" => HashTable => {"contains entries of the form ", TT "i=>NIi"}
	  },
     "The derived restriction modules of a D-module M are
     the derived inverse images in the sense of algebraic
     geometry but in the category of D-modules. 
     This routine computes restrictions to coordinate subspaces,
     where the subspace is determined
     by the strictly positive entries of the weight vector", EM "w", ",
     e.g., ", EM "{x_i = 0 : w_i > 0}", " if ", 
     EM "D = ", BOLD "C", EM "<x_1,...,x_n,d_1,...,d_n>", ".
     The input weight vector should be a list of ", EM "n", " numbers
     to induce the weight ", EM "(-w,w)", " on ", EM "D", ".",

     PARA {
	  "The algorithm used appears in the paper 'Algorithms for D-modules'
	  by Oaku-Takayama(1999).  The method is to compute an adapted resolution
	  with respect to the weight vector w and use the b-function with respect
	  to w to truncate the resolution."},
     EXAMPLE lines ///
	     R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
	     I = ideal(x_1, D_2-1) 
	     Drestriction(I,{1,0})
	     ///,
     Caveat =>{"The module ", EM "M", " should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector ", EM "w", " should be a list of ", EM "n",
	  " numbers if ", EM "M", 
	  " is a module over the ", EM "n", "-th Weyl algebra."},
     SeeAlso => {"DrestrictionAll", "DrestrictionClasses", "DrestrictionComplex", 
	  "DrestrictionIdeal", "Dresolution", "Dintegration"}
     }

document {
     Key => {DrestrictionIdeal, (DrestrictionIdeal, Ideal, List)},
     Headline => "restriction ideal of a D-module",
     Usage => "DrestrictionIdeal(I,w)",
     Inputs => {
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  Ideal => {"the restriction ideal of ", EM "M", " w.r.t. the weight vector ", EM "w"}
	  },
     "A supplementary function for ", TO "Drestriction", 
     " that computes the restriction ideal.",   
     EXAMPLE lines ///
          W = QQ[y,t,Dy,Dt, WeylAlgebra => {y=>Dy, t=>Dt}];
     	  I = ideal(2*t*Dy+Dt, t*Dt+2*y*Dy+2); -- annihilator of 1/(t^2-y)
     	  DrestrictionIdeal(I, {1,4})
	  ///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Drestriction", "DrestrictionAll", "Dintegration"}
}

document {
     Key => {DrestrictionAll, (DrestrictionAll, Module, List), (DrestrictionAll, Ideal, List)},
     Headline => "restriction modules of a D-module (extended version)",
     Usage => "N = DrestrictionAll(M,w), NI = DrestrictionAll(I,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  "N" => HashTable, 
     	  "NI" => HashTable
	  },
     "An extension of ", TO "Drestriction", 
     " that computes the restriction complex, restriction classes, etc.",   
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	DrestrictionAll(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Drestriction", "DrestrictionClasses", "DrestrictionComplex", 
	  "DrestrictionIdeal", "Dintegration"}
}

document {
     Key => {DrestrictionComplex, (DrestrictionComplex, Module, List), (DrestrictionComplex, Ideal, List)},
     Headline => "derived restriction complex of a D-module",
     Usage => "N = DrestrictionComplex(M,w), NI = DrestrictionComplex(I,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  "N" => HashTable, 
     	  "NI" => HashTable 
	  },
     "An extension of ", TO "Drestriction", 
     " that computes the derived restriction complex.",   
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	DrestrictionComplex(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Drestriction", "DrestrictionAll", "Dintegration"}
}


document {
     Key => {DrestrictionClasses, (DrestrictionClasses,ZZ,Module,List), (DrestrictionClasses,Ideal,List), (DrestrictionClasses,Module,List),
      (DrestrictionClasses,ZZ,Ideal,List)},
     Headline => "restriction classes of a D-module",
     Usage => "N = DrestrictionClasses(M,w), NI = DrestrictionClasses(I,w), Ni = DrestrictionClasses(i,M,w),
     NIi = DrestrictionClasses(i,I,w), ",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector",
	  "i" => ZZ => "nonnegative"  
	  },
     Outputs => {
	  "Ni" => HashTable,
     	  "N" => HashTable,
	  "NIi" => HashTable,
	  "NI" => HashTable
	  },
     "An extension of ", TO "Drestriction", 
     " that computes the explicit cohomology classes of a derived restriction complex.",        
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	DrestrictionClasses(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Drestriction", "DrestrictionAll", "Dintegration"}
}

document {
     Key => HomologyModules,
     Headline => "a key in a hashtable; an option of DExt",
     SeeAlso => {"Drestriction", "Dintegration", "DExt" }
     }
document {
     Key => GenCycles,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }
document {
     Key => Exponents,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }
document {
     Key => Cycles,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }
document {
     Key => Boundaries,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }
document {
     Key => VResolution,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }
document {
     Key => BFunction,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }
document {
     Key => Explicit,
     Headline => "a key in the hashtable created by Drestriction/Dintegration",
     SeeAlso => {"Drestriction", "Dintegration" }
     }

document {
     Key => IntegrateComplex,
     Headline => "a key in the hashtable created by Dintegration",
     SeeAlso => {"Dintegration" }
     }

document {
     Key => [Dintegration,Strategy],
	  "Option is passed to Dresolution. See ",
	  TO [Dresolution,Strategy]
}

document {
     Key => { Dintegration, (Dintegration,ZZ,Module,List), (Dintegration,Ideal,List), 
	  (Dintegration,Module,List), (Dintegration,ZZ,Ideal,List) },
     Headline => "integration modules of a D-module",
     Usage => "N = Dintegration(M,w), NI = Dintegration(I,w), Ni = Dintegration(i,M,w),
     NIi = Dintegration(i,I,w), ",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector",
	  "i" => ZZ => "nonnegative"  
	  },
     Outputs => {
	  "Ni" => Module => {"the i-th derived integration module of ", EM "M"," with respect
     to the weight vector ", EM "w"},
     	  "N" => HashTable => {"contains entries of the form ", TT "i=>Ni"},
	  "NIi" => Module => {"the i-th derived integration module of ", EM "D/I", " with respect
     to the weight vector ", EM "w"},
     	  "NI" => HashTable => {"contains entries of the form ", TT "i=>NIi"}
	  },
     "The derived integration modules of a D-module ", EM "M", " are
     the derived direct images in the category of D-modules. 
     This routine computes integration for projection to 
     coordinate subspaces, where the subspace is determined
     by the strictly positive entries of the weight vector ", EM "w", ",
     e.g., ", EM "{x_i = 0 : w_i > 0}", " if ", 
     EM "D = ", BOLD "C", EM "<x_1,...,x_n,d_1,...,d_n>", ".
     The input weight vector should be a list of ", EM "n", " numbers	    
     to induce the weight ", EM "(-w,w)", " on ", EM "D", ".",
     PARA "",
     "The algorithm used appears in the paper 'Algorithms for D-modules'
     by Oaku-Takayama(1999).  The method is to take the Fourier transform
     of M, then compute the derived restriction, then inverse
     Fourier transform back.",
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	Dintegration(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"DintegrationAll", "DintegrationClasses", "DintegrationComplex", 
	  "DintegrationIdeal", "Drestriction"}
 }

document {
     Key => {DintegrationIdeal, (DintegrationIdeal, Ideal, List)},
     Headline => "integration ideal of a D-module",
     Usage => "DintegrationIdeal(I,w)",
     Inputs => {
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  Ideal => {"the integration ideal of ", EM "M", " w.r.t. the weight vector ", EM "w"}
	  },
     "A supplementary function for ", TO "Dintegration", 
     " that computes the integration ideal.",   
     EXAMPLE lines ///
          W = QQ[y,t,Dy,Dt, WeylAlgebra => {y=>Dy, t=>Dt}];
     	  I = ideal(2*t*Dy+Dt, t*Dt+2*y*Dy+2); -- annihilator of 1/(t^2-y)
     	  DintegrationIdeal(I, {1,4})
	  ///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Dintegration", "DintegrationAll", "Drestriction"}
}

document {
     Key => {DintegrationAll, (DintegrationAll, Module, List), (DintegrationAll, Ideal, List)},
     Headline => "integration modules of a D-module (extended version)",
     Usage => "N = DintegrationAll(M,w), NI = DintegrationAll(I,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  "N" => HashTable, 
     	  "NI" => HashTable
	  },
     "An extension of ", TO "Dintegration", 
     " that computes the integration complex, integration classes, etc.",   
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	DintegrationAll(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Dintegration", "DintegrationClasses", "DintegrationComplex", 
	  "DintegrationIdeal", "Drestriction"}
}

document {
     Key => {DintegrationComplex, (DintegrationComplex, Module, List), (DintegrationComplex, Ideal, List)},
     Headline => "derived integration complex of a D-module",
     Usage => "N = DintegrationComplex(M,w), NI = DintegrationComplex(I,w)",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector"
	  },
     Outputs => {
     	  "N" => HashTable, 
     	  "NI" => HashTable 
	  },
     "An extension of ", TO "Dintegration", 
     " that computes the derived integration complex.",   
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	DintegrationComplex(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Dintegration", "DintegrationAll", "Drestriction"}
}


document {
     Key => {DintegrationClasses, (DintegrationClasses,ZZ,Module,List), (DintegrationClasses,Ideal,List), (DintegrationClasses,Module,List),
      (DintegrationClasses,ZZ,Ideal,List)},
     Headline => "integration classes of a D-module",
     Usage => "N = DintegrationClasses(M,w), NI = DintegrationClasses(I,w), Ni = DintegrationClasses(i,M,w),
     NIi = DintegrationClasses(i,I,w), ",
     Inputs => {
	  "M" => Module => {"over the Weyl algebra ", EM "D"},
	  "I" => Ideal => {"which represents the module ", EM "M = D/I"},
	  "w" => List => "a weight vector",
	  "i" => ZZ => "nonnegative"  
	  },
     Outputs => {
	  "Ni" => HashTable,
     	  "N" => HashTable,
	  "NIi" => HashTable,
	  "NI" => HashTable
	  },
     "An extension of ", TO "Dintegration", 
     " that computes the explicit cohomology classes of a derived integration complex.",        
     EXAMPLE lines ///
	R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]
    	I = ideal(x_1, D_2-1) 
     	DintegrationClasses(I,{1,0})
	///,
     Caveat =>{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},
     SeeAlso =>{"Dintegration", "DintegrationAll", "Drestriction"}
}
