needs "D-modules.m2"

document { Drestriction,
     TT "Drestriction (M, w)", " -- 
     computes derived restriction modules of M with respect
     to the weight vector w",
     BR, NOINDENT,
     TT "Drestriction (I, w)", " -- 
     computes derived restriction modules of D/I with respect
     to the weight vector w",
     BR, NOINDENT,
     TT "Drestriction (i, M, w)", " -- 
     computes i-th derived restriction module of M with respect
     to the weight vector w",
     BR, NOINDENT,
     TT "Drestriction (i, I, w)", " -- 
     computes i-th derived restriction module of D/I with respect
     to the weight vector w",
     PARA,
     TT "DrestrictionClasses (M, w)", " -- 
     computes explicit cohomology classes of a
     derived restriction complex of M",
     BR, NOINDENT,
     TT "DrestrictionClasses (I, w)", " -- 
     computes explicit cohomology classes of a
     derived restriction complex of D/I",
     BR, NOINDENT,
     TT "DrestrictionClasses (i, M, w)", " -- 
     computes i-th explicit cohomology classes of
     a derived restriction complex of M",
     BR, NOINDENT,
     TT "DrestrictionClasses (i, I, w)", " -- 
     computes i-th explicit cohomology classes of
     a derived restriction complex of D/I",
     PARA,
     TT "DrestrictionComplex (M, w)", " -- 
     computes derived restriction complex of M with respect
     to the weight vector w",
     BR, NOINDENT,
     TT "DrestrictionComplex (D/I, w)", " -- 
     computes derived restriction complex of D/I with respect
     to the weight vector w",
     PARA,
     TT "DrestrictionAll (M, w)", " -- 
     computes derived restriction of M and outputs various
     information",
     BR, NOINDENT,
     TT "DrestrictionAll (I, w)", " -- 
     computes derived restriction of D/I and outputs various
     information",

     PARA,
     "The derived restriction modules of a D-module M are
     the derived inverse images in the sense of algebraic
     geometry but in the category of D-modules. 
     This routine computes restrictions to coordinate subspaces,
     where the subspace is determined
     by the strictly positive entries of the weight vector 'w',
     e.g. {x_i = 0 : w_i > 0} if D = C<x_1,d_1,...,x_n,d_n>.
     The input weight vector should be a list of n numbers
     to induce the weight (-w,w) on D.",

     PARA,
     "The algorithm used appears in the paper 'Algorithims for D-modules'
     by Oaku-Takayama(1999).  The method is to compute an adapted resolution
     with respect to the weight vector w and use the b-function with respect
     to w to truncate the resolution.",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]",
     EXAMPLE "I = ideal(x_1, D_2-1)", 
     EXAMPLE "Drestriction(I,{1,0})",

     PARA,
     "Caveats and known problems :",
     MENU{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},

     "Optional arguments :",
     MENU{"Drestriction (..., Strategy => Schreyer,...) -- 
	  option passed to Dresolution",
	  "Drestriction (..., Strategy => Vhomogenize,...) -- 
	  option passed to Dresolution"},

     PARA,
     "Abbreviations :",
     MENU{"Drestrict"},
     
     SEEALSO {"Dresolution", "Dintegration"}

--     "Ways to use ", TO "Drestriction",
--     MENU {"Drestriction (Ideal, List) -- returns hash table of all derived 
--	  restriction modules of D/I", "Drestriction (Module, List)",
--	  "Drestriction (ZZ, Ideal, List) -- returns i-th derived 
--	  restriction module of D/I", "Drestriction (ZZ, Module, List)"
--	  },
--     MENU {"DrestrictionClasses (Ideal, List) -- returns hash table of explicit
--	  cohomology classes in a restriction complex of D/I",
--	  "DrestrictionClasses (Module, List)",
--	  "DrestrictionClasses (ZZ, Ideal, List) -- returns
--	  cohomology classes of the i-th derived 
--	  restriction module of D/I", 	  "DrestrictionClasses (ZZ, Module, List)"
--	  },
--     MENU {"DrestrictionComplex (Ideal, List) -- returns a finitely
--	  generated restriction complex of D/I",
--	  "DrestrictionComplex (Module, List)"
--	  },
--     MENU {"DrestrictionAll (Ideal, List) -- returns derived restriction modules
--	  as well as extra information",
--	  "DrestrictionAll (Module, List)"
--	  },
     
--     "See also :",     
--     MENU{HREF{"/HOME/Dresolution.html","Dresolution"},
--	  HREF{"/HOME/Dintegration.html","Dintegration"}}
     }

document { Dintegration,
     TT "Dintegration (M, w)", " -- 
     computes derived integration modules of M with respect
     to the weight vector w",
     BR, NOINDENT,
     TT "Dintegration (I, w)", " -- 
     computes derived integration modules of D/I with respect
     to the weight vector w",
     BR, NOINDENT,
     TT "Dintegration (i, M, w)", " -- 
     computes i-th derived integration module of M with respect
     to the weight vector w",
     BR, NOINDENT,
     TT "Dintegration (i, I, w)", " -- 
     computes i-th derived integration module of D/I with respect
     to the weight vector w",
     PARA,
     TT "DintegrationClasses (M, w)", " -- 
     computes explicit cohomology classes of a
     derived integration complex of M",
     BR, NOINDENT,
     TT "DintegrationClasses (I, w)", " -- 
     computes explicit cohomology classes of a
     derived integration complex of D/I",
     BR, NOINDENT,
     TT "DintegrationClasses (i, M, w)", " -- 
     computes i-th explicit cohomology classes of
     a derived integration complex of M",
     BR, NOINDENT,
     TT "DintegrationClasses (i, I, w)", " -- 
     computes i-th explicit cohomology classes of
     a derived integration complex of D/I",
     PARA,
     TT "DintegrationComplex (M, w)", " -- 
     computes derived integration complex of M with respect
     to the weight vector w",
     BR, NOINDENT,
     TT "DintegrationComplex (D/I, w)", " -- 
     computes derived integration complex of D/I with respect
     to the weight vector w",
     PARA,
     TT "DintegrationAll (M, w)", " -- 
     computes derived integration of M and outputs various
     information",
     BR, NOINDENT,
     TT "DintegrationAll (I, w)", " -- 
     computes derived integration of D/I and outputs various
     information",

     PARA,
     "The derived integration modules of a D-module M are
     the derived direct images in the category of D-modules. 
     This routine computes integration for projection to 
     coordinate subspaces, where the subspace is determined
     by the strictly positive entries of the weight vector 'w',
     e.g. {x_i = 0 : w_i > 0} if D = C<x_1,d_1,...,x_n,d_n>.
          The input weight vector should be a list of n numbers
     to induce the weight (-w,w) on D.",

     PARA,
     "The algorithm used appears in the paper 'Algorithims for D-modules'
     by Oaku-Takayama(1999).  The method is to take the Fourier transform
     of M, then compute the derived restriction, then inverse
     Fourier transform back.",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]",
     EXAMPLE "I = ideal(x_1, D_2-1)", 
     EXAMPLE "-- Dintegration(I,{1,0})",		    -- didn't work (drg)

     PARA,
     "Caveats and known problems :",
     MENU{"The module M should be specializable to the subspace.
	  This is true for holonomic modules.",
	  "The weight vector w should be a list of n numbers if M
	  is a module over the nth Weyl algebra."},


     "Optional arguments :",
     MENU{"Dintegration (..., Strategy => Schreyer,...) -- option passed to Dresolution",
	  "Dintegration (..., Strategy => Vhomogenize,...) -- 
	  option passed to Dresolution"},

     PARA,
     "Abbreviations :",
     MENU{"Dintegrate"},
     
     SEEALSO{"Drestriction"}

--     "Ways to use ", TO "Dintegration",
--     MENU {"Dintegration (Ideal, List) -- returns hash table of all derived 
--	  integration modules of D/I", "Dintegration (Module, List)",
--	  "Dintegration (ZZ, Ideal, List) -- returns i-th derived 
--	  integration module of D/I", "Dintegration (ZZ, Module, List)"
--	  },
--     MENU{"DintegrationClasses (Ideal, List) -- returns hash table of explicit
--	  cohomology classes in a integration complex of D/I", 
--	  "DintegrationClasses (Module, List)",
--	  "DintegrationClasses (ZZ, Ideal, List) -- returns
--	  cohomology classes of the i-th derived 
--	  integration module of D/I", 	  "DintegrationClasses (ZZ, Module, List)"
--	  },
--     MENU{"DintegrationComplex (Ideal, List) -- returns a finitely
--	  generated integration complex of D/I",
--	  "DintegrationComplex (Module, List)"
--	  },
--     MENU{"DintegrationAll (Ideal, List) -- returns derived integration modules
--	  as well as extra information",
--	  "DintegrationAll (Module, List)"
--	  },

--     MENU{HREF{"/HOME/Drestriction.html","Drestriction"},
--	  HREF{"/HOME/Dresolution.html","Dresolution"}}
     }
