-- -*- coding: utf-8 -*-
--		Copyright 1993-2007 by Daniel R. Grayson 

document {
     Key => dd,
     Headline => "differential in a chain complex",
     TT "dd", " -- a symbol used as a key in a chain complex, under which
     are stored the differentials.",
     PARA{},
     "The differentials are stored as the members in a chain complex
     map of degree ", TT "-1", ".  The map ", TT "C.dd_i", " is the
     one whose source is the module ", TT "C_i", ", and it is arranged to
     be zero if no map has been stored in that spot.",
     EXAMPLE {
	  "R = ZZ/101[a,b];",
      	  "C = resolution cokernel vars R",
      	  "C.dd",
      	  "C.dd_2",
	  },
     SeeAlso => "ChainComplex"
     }

document {
     Key => GradedModule,
     Headline => "the class of all graded modules",
     "A new graded module can be made with 'M = new GradedModule'.
     The i-th module can be installed with a statement like ", TT "M#i=N", ",
     and can be retrieved with an expression like ", TT "M_i", ".  The ground
     ring should be installed with a statement like ", TT "M.ring = R", ".",
     SeeAlso => "GradedModuleMap",
    Subnodes => {
	TO (gradedModule, List),
	TO (complete, GradedModule),
	TO (symbol SPACE, GradedModule, Array),
        TO (max, GradedModule),
        TO (min, GradedModule),
        TO (symbol **, GradedModule, GradedModule),
        TO (symbol **, ChainComplex, GradedModule),
        TO (symbol **, GradedModule, ChainComplex),
        TO (symbol **, GradedModule, Module),
        TO (length, GradedModule),
	TO (betti, ChainComplex),
        },
     }

document {
     Key => GradedModuleMap,
     Headline => "the class of all maps between graded modules",
     SeeAlso => "GradedModule",
    Subnodes => {
	TO gradedModuleMap,
	TO (symbol |, GradedModuleMap, GradedModuleMap),
        TO (symbol ||, GradedModuleMap, GradedModuleMap),
        TO (source, GradedModuleMap),
        TO (target, GradedModuleMap),
        },
     }

document {
    Key => {
	(gradedModule, List),
	(gradedModule, Module),
	(gradedModule, Sequence),
	(gradedModule, ChainComplex),
    },
     Headline => "make a graded module",
     Usage => "gradedModule v",
     Inputs => { "v" => List => "a module, or a list or sequence of modules" },
     Outputs => {{"the graded module with the ", TT "i", "-th element of ", TT "v", " installed as its ", TT "i", "-th component"}},
     EXAMPLE lines ///
     	  gradedModule ZZ^2
     	  gradedModule(ZZ^2,ZZ^3,ZZ^400)
     ///,
     "If ", TT "v", " is ", ofClass ChainComplex, " then the return value is the graded module underlying it.",
     EXAMPLE lines ///
     	  R = QQ[x,y]
	  C = res coker vars R
	  gradedModule C
     ///,
     }

document {
     Key => {gradedModuleMap,(gradedModuleMap, Sequence), (gradedModuleMap, Matrix), (gradedModuleMap, List)},
     Headline => "make a map of graded modules",
     Usage => "gradedModuleMap v",
     Inputs => { "v" => List => "a matrix, or a list or sequence of matrices" },
     Outputs => {{"the graded module map with the ", TT "i", "-th element of ", TT "v", " installed as its ", TT "i", "-th component"}},
     EXAMPLE lines ///
     	  gradedModuleMap(random(ZZ^3,ZZ^4),random(ZZ^2,ZZ^2))
     ///
     }

document {
     Key => ChainComplex,
     Headline => "the class of all chain complexes",
     "For an overview of creating and using chain complexes in Macaulay2, see ",
     TO "chain complexes", ".",
     PARA{},
     "Common ways to create a chain complex",
     UL {
	  TO chainComplex,
	  TO (resolution,Module),
	  },
     "Information about a chain complex",
     UL {
	  TO (length,ChainComplex),
	  TO (min,GradedModule),
	  TO (max,GradedModule),
	  TO (betti,GradedModule),
	  TO (ring,ChainComplex)
	  },
     "Operations on chain complexes",
     UL {
	  TO (dual,ChainComplex),
	  TO (symbol++,ChainComplex,ChainComplex),
	  TO (symbol**,ChainComplex,Module),
	  TO (symbol**,ChainComplex,ChainComplex),
	  TO (Hom,ChainComplex,Module),
	  TO (symbol SPACE, ChainComplex, Array)
	  },
     SeeAlso => {
	 (map,ChainComplex,ChainComplex,Function),
         },
     Subnodes => {
	 TO chainComplex,
	 -- TO (NewFromMethod, ChainComplex, Resolution),
	TO dd,
	TO (status, ChainComplex),
        TO (complete, ChainComplex),
        TO (length, ChainComplex),
        TO (symbol ++, ChainComplex, ChainComplex),
        TO (components, ChainComplex),
        TO (symbol SPACE, ChainComplex, Array),
        TO (dual, ChainComplex),
        TO (sum, ChainComplex),
        TO (NewMethod, ChainComplex),
        TO (symbol **, ChainComplex, ChainComplex),
        TO (symbol **, ChainComplex, Ring),
        TO (cohomology, ZZ, ChainComplex),
        TO (homology, ChainComplex),
        TO (homology, ZZ, ChainComplex),
        TO (poincare, ChainComplex),
        TO (poincareN, ChainComplex),
	TO (regularity, ChainComplex),
        TO (symbol ^, ChainComplex, ZZ),
        TO (symbol _, ChainComplex, ZZ),
        TO ((symbol _, symbol =), ChainComplex, ZZ),
	TO (symbol ^, ChainComplex, Array),
	TO (symbol _, ChainComplex, Array),
        },
     }

document {
    Key => {
	(complete, GradedModule),
	(complete, ChainComplexMap),
    },
     TT "complete C", " -- completely fills out the chain complex C by
     calling upon the engine to provide the maps and modules computed
     by ", TO "resolution", ".",
     PARA{},
     "This is mainly intended for developers of new routines for chain
     complexes that have to make use of their internal structure.
     Before running this routine, it is not possible to determine which
     spots in a chain complex are actually occupied by modules or maps."
     }

document {
     Key => (complete, ChainComplex),
     Headline => "complete the internal parts",
     TT "complete C", " -- fills in the modules of a chain complex
     obtained as a resolution with information from the engine.",
     PARA{},
     "For the sake of efficiency, when a chain complex arises as
     a resolution of a module, the free modules are not filled in until
     they are needed.  This routine can be used to fill them all in, and
     is called internally when a chain complex is printed.
     Normally users will not need this function, unless they use ", TO "#", " to
     obtain the modules of the chain complex, or use ", TO "keys", " to
     see which spots are occupied by modules.",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "C = resolution cokernel vars R;",
      	  "keys C",
      	  "complete C;",
      	  "keys C"
	  }
     }


document {
     Key => (length, ChainComplex),
     Headline => "length of a chain complex or graded module",
     TT "length C", " -- the length of a chain complex.",
     PARA{},
     "The length of a chain complex is defined to be the difference
     between the smallest and largest indices of spots occupied by
     modules, even if those modules happen to be zero."
     }

document {
     Key => ChainComplexMap,
     Headline => "the class of all maps between chain complexes",
     "The usual algebraic operations are available: addition, subtraction,
     scalar multiplication, and composition.  The identity map from a
     chain complex to itself can be produced with ", TO "id", ".  An
     attempt to add (subtract, or compare) a ring element to a chain complex
     will result in the ring element being multiplied by the appropriate
     identity map.",
    Subnodes => {
	TO (cone, ChainComplexMap),
        TO (symbol SPACE, ChainComplexMap, Array),
        TO (sum, ChainComplexMap),
        TO (symbol **, ChainComplexMap, ChainComplex),
        TO (symbol **, ChainComplex, ChainComplexMap),
        TO (symbol **, ChainComplexMap, ChainComplexMap),
        TO (dual, ChainComplexMap),
        TO (cohomology, ZZ, ChainComplexMap),
        TO (homology, ZZ, ChainComplexMap),
        TO (homology, ChainComplexMap),
        TO (kernel, ChainComplexMap),
        TO (source, ChainComplexMap),
        TO (target, ChainComplexMap),
        TO (transpose, ChainComplexMap),
        TO (symbol ^, ChainComplexMap, ZZ),
        TO (symbol _, ChainComplexMap, ZZ),
	TO (symbol _, ChainComplexMap, Array),
        TO ((symbol _, symbol =), ChainComplexMap, ZZ),
	TO (extend,ChainComplex,ChainComplex,Matrix),
	TO (Hom,Module,ChainComplex),
	TO (map,ChainComplex,ChainComplex,Function),
	TO nullhomotopy,
        },
     }

document {
     Key => {(extend,ChainComplex,ChainComplex,Matrix),[extend,Verify]},
     Usage => "extend(D,C,f0)",
     Inputs => {
	  "D",
	  "C",
	  "f0" => { "a map from ", TT "C_0", " to ", TT "D_0" },
	  Verify => Boolean => {"whether to check that the map extends"}
	  },
     Outputs => { { "a chain complex map ", TT "f: D <--- C", " of degree 0 extending ", TT "f0", " in the sense that ", TT "f_0==f0" } },
     EXAMPLE {
	  "R = ZZ/101[a..c]",
	  "I = image vars R",
	  "J = image symmetricPower (2,vars R)",
	  "g = extend( resolution (R^1/I), resolution (R^1/J), id_(R^1))",
	  "g_1",
	  "g_2"
	  },
     SeeAlso => {(symbol _,ChainComplex,ZZ),(symbol _,ChainComplexMap,ZZ),(cone,ChainComplexMap) }
     }

document {
     Key => (cone,ChainComplexMap),
     Usage => "cone f",
     Inputs => { "f" },
     Outputs => { {"the mapping cone of a ", TT "f", ""} },
     {
	  EXAMPLE {
	       "R = ZZ/101[x,y,z]",
	       "m = image vars R",
	       "m2 = image symmetricPower(2,vars R)",
	       "M = R^1/m2",
	       "N = R^1/m",
	       "C = cone extend(resolution N,resolution M,id_(R^1))",
	       },
	  "Let's check that the homology is correct; for example, ", TT "HH_0", " should be zero.",
	  EXAMPLE "prune HH_0 C",
	  "Let's check that ", TT "HH_1", " is isomorphic to ", TT "m/m2", ".",
	  EXAMPLE {
	       "prune HH_1 C",
	       "prune (m/m2)"
	       }
	  }}

document {
     Key => {nullhomotopy,(nullhomotopy, ChainComplexMap)},
     Headline => "make a null homotopy",
     TT "nullhomotopy f", " -- produce a nullhomotopy for a map f of 
     chain complexes.",
     PARA{}, 
     "Whether f is null homotopic is not checked.",
     PARA{},
     "Here is part of an example provided by Luchezar Avramov.  We
     construct a random module over a complete intersection, resolve 
     it over the polynomial ring, and produce a null homotopy for the
     map that is multiplication by one of the defining equations
     for the complete intersection.",
     EXAMPLE {
	  "A = ZZ/101[x,y];",
      	  "M = cokernel random(A^3, A^{-2,-2})",
      	  "R = cokernel matrix {{x^3,y^4}}",
      	  "N = prune (M**R)",
      	  "C = resolution N",
      	  "d = C.dd",
      	  "s = nullhomotopy (x^3 * id_C)",
      	  "s*d + d*s",
      	  "s^2"
	  },
     }

document {
     Key => {(symbol ++,ChainComplex,ChainComplex),(symbol ++,GradedModule,GradedModule),
	  (symbol ++,ChainComplexMap,ChainComplexMap)},
     Headline => "direct sum",
     TT "C++D", " -- direct sum of chain complexes and maps",
     EXAMPLE lines ///
	  C = resolution cokernel matrix {{4,5}}
      	  C ++ C[-2]
     ///
     }

document {
     Key => (components, ChainComplex),
     TT "components C", " -- returns from which C was formed, if C
     was formed as a direct sum.",
     PARA{},
     SeeAlso => "isDirectSum"
     }

document {
     Key => (symbol SPACE, ChainComplex, Array),
     Headline => "degree shift",
     Usage => "D = C[i]",
     Inputs => {
	  "C" => {},
	  "i" => Array => {"an array ", TT "[i]", " containing an integer ", TT "i"}
	  },
     Outputs => {
	  "D" => {"a new chain complex ", TT "D", " in which ", TT "D_j", " is ", TT "C_(i+j)", ".  The signs of the
	       differentials are reversed if ", TT "i", " is odd."}
	  },
     EXAMPLE lines ///
     R = QQ[x..z];
     C = res coker vars R
     C[3]
     ///
     }

document {
     Key => (symbol SPACE, ChainComplexMap, Array),
     Headline => "degree shift",
     Usage => "g = f[i]",
     Inputs => {
	  "f" => {},
	  "i" => Array => {"an array ", TT "[i]", " containing an integer ", TT "i"}
	  },
     Outputs => {
	  "g" => {"a new chain complex map ", TT "g", " in which ", TT "g_j", " is ", TT "f_(i+j)", "."}
	  },
     EXAMPLE lines ///
     R = QQ[x..z];
     C = res coker vars R;
     f = id_C
     f[3]
     ///
     }

document {
     Key => (symbol SPACE, GradedModule, Array),
     Headline => "degree shift",
     TT "C[i]", " -- shifts the graded module ", TT "C", ", producing a new graded module
     ", TT "D", " in which ", TT "D_j", " is ", TT "C_(i+j)", "."
     }

undocumented {
    [(dual, ChainComplex), Strategy],
    [(dual, ChainComplex), DegreeLimit],
    [(dual, ChainComplex), MinimalGenerators],
}
document {
     Key => (dual, ChainComplex),
     Headline => "dual",
     TT "dual C", " -- the dual of a chain complex."
     }

document {
     Key => syzygyScheme,
     Headline => "construct a syzygy scheme",
     TT "syzygyScheme(C,i,v)", " -- produce the syzygy scheme from a map
     ", TT "v : R^j ---> C_i", " which selects some syzygies from a resolution ", TT "C", "."
     }

document {
     Key => (sum, ChainComplex),
     Headline => "direct sum of the components of a chain complex",
     TT "sum C", " -- yields the sum of the modules in a chain complex.",
     PARA{},
     "The degrees of the components are preserved.",
     EXAMPLE {
	  "R = ZZ/101[a..d];",
      	  "C = res coker vars R",
      	  "sum C",
      	  "degrees oo",
	  },
     SeeAlso => {"sum",(sum, ChainComplexMap)}
     }

document {
     Key => (sum, ChainComplexMap),
     Headline => "direct sum of the components of a chain map",
     TT "sum C", " -- yields the sum of the modules in a chain complex map.",
     PARA{},
     "The degrees of the components are preserved.",
     EXAMPLE {
	  "R = ZZ/101[a..c];",
      	  "C = res coker vars R",
      	  "sum C.dd",
      	  "betti oo",
	  },
     SeeAlso => {"sum", (sum, ChainComplex)}
     }

document {
     Key => (NewMethod, ChainComplex),
     Headline => "make a new chain complex from scratch",
     TT "C = new ChainComplex", " -- make a new chain complex.",
     PARA{},
     "The new chain complex is initialized with a differential of
     degree ", TT "-1", " accessible as ", TT "C.dd", " and of type
     ", TO "ChainComplexMap", ".  You can take the new chain complex and
     fill in the ring, the modules, and the differentials.",
     EXAMPLE {
	  "C = new ChainComplex;",
      	  "C.ring = ZZ;",
      	  "C#2 = ZZ^1;",
      	  "C#3 = ZZ^2;",
      	  "C.dd#3 = matrix {{3,-11}};",
      	  "C",
      	  "C.dd"
	  },
     }

document {
     Key => {(symbol **, ChainComplex, ChainComplex), (symbol**, ChainComplex, Module),(symbol**, Module, ChainComplex)},
     Headline => "tensor product",
     TT "C**D", " -- the tensor product of two chain complexes.",
     PARA{},
     "The result, ", TT "E", ", is a chain complex.  Each module ", TT "E_k", " 
     in it is a direct sum of modules of the form ", TT "C_i**D_j", " with
     ", TT "i+j=k", ", and the preferred keys for the components of this direct
     sum are the pairs ", TT "(i,j)", ", sorted increasing values of ", TT "i", ".  For
     information about how to use preferred keys, see ", TO "directSum", "."
     }

document {
     Key => (symbol **, ChainComplex, GradedModule),
     Headline => "tensor product",
     TT "C**D", " -- the tensor product of a chain complex with a graded module.",
     PARA{},
     "The result is a chain complex."
     }

document {
     Key => (symbol **, GradedModule, ChainComplex),
     Headline => "tensor product",
     TT "C**D", " -- the tensor product of a graded module with a chain complex.",
     PARA{},
     "The result is a chain complex."
     }

document {
     Key => {(symbol **, ChainComplexMap, ChainComplex),(symbol**, ChainComplexMap, Module)},
     Headline => "tensor product",
     TT "f ** C", " -- tensor product of a map of chain complexes with a chain complex.",
     PARA{},
     SeeAlso => "ChainComplexMap"
     }

document {
     Key => {(symbol **, ChainComplex, ChainComplexMap),(symbol**, Module, ChainComplexMap)},
     Headline => "tensor product",
     TT "C ** f", " -- tensor product of a chain complex with a map of chain complexes.",
     PARA{},
     SeeAlso => "ChainComplexMap"
     }

document {
     Key => (symbol **,ChainComplex,Ring),
     Usage => "C ** R",
     Inputs => {"C","R"},
     Outputs => {{"the tensor product of ", TT "C", " with ", TT "R" }},
     EXAMPLE lines ///
     R = QQ[a..d];
     C = res coker vars R
     S = R[x]
     C**S
     ///
     }

document {
     Key => (symbol **, ChainComplexMap, ChainComplexMap),
     Headline => "tensor product",
     TT "f ** g", " -- tensor product of two maps of chain complexes.",
     SeeAlso => "ChainComplexMap"
     }

document {
     Key => (dual,ChainComplexMap),
     Headline => "dual of a chain complex",
     Usage => "dual C",
     Inputs => { "C" },
     Outputs => { { "the dual of the chain complex ", TT "C", "" } },
     EXAMPLE {
	  "R = QQ[a..f]",
	  "M = coker genericMatrix(R,a,2,3)",
	  "res M",
	  "dual oo"
	  }
     }



-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
