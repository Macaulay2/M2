needs "D-modules.m2"

document { Dresolution,
     TT "Dresolution (M)", " -- 
     computes a Schreyer resolution of the D-module M",
     BR, NOINDENT, TT "Dresolution (I, w)", " -- computes
     a Schreyer resolution of the quotient module D/I",
     PARA,
     TT "Dresolution (M, w)", "-- 
     computes a resolution of M adapted to a weight vector w
     of the form (-u,u)",
     BR, NOINDENT, TT "Dresolution (I, w)", "-- 
     computes a resolution of D/I adapted to a weight vector w
     of the form (-u,u)",

     PARA,
     "This routine computes various resolutions of a D-module.
     If no weight vector is specified, then the command
     produces a resolution by using the Schreyer order implemented
     in the engine.  If a weight vector of the form (-u,u) is specified,
     then the command produces a resolution with shifts
     which is adapted to the weight vector (-u,u).  
     These w-adapted resolutions are compatible
     with b-functions and used in the restriction algorithm.
     For ordinary resolutions, the user may use the command 'resolution'.
     Note that there is not a well-defined notion
     of minimality for resolutions over D.",

     PARA,
     "There are two strategies for constructing
     w-adapted resolutions.   The first strategy is to construct
     a Schreyer resolution in the homogenized Weyl algebra
     and then dehomogenize.  The second strategy is to homogenize
     with respect to the weight vector.
     These strategies are described in the paper
     'Algorithims for D-modules'
     by Oaku-Takayama(1999).",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "R = QQ[x_1,x_2,D_1,D_2,WeylAlgebra=>{x_1=>D_1,x_2=>D_2}]",
     EXAMPLE "I = ideal(x_1*D_1+3*x_2*D_2-1, D_1^3-D_2)", 
     EXAMPLE "Dresolution(I,{-1,-1,1,1})",

     PARA,
     "Optional arguments :",
     
     MENU{"Dresolution (..., Strategy => Schreyer,...) -- uses
	  Schreyer method in homogeneous Weyl algebra",
	  "Dresolution (..., Strategy => Vhomogenize,...) -- uses
	  V-homogenization method of Oaku"},

     PARA,
     "Abbreviations :",
     MENU{"Dres"},

     SEEALSO {"gbw", "Drestriction"}

--     "Ways to use ", TO "Dresolution",
--     MENU {"Dresolution (Ideal) -- returns a Schreyer resolution of the
--	  ideal I", "Dresolution (Module) -- returns Schreyer resolution
--	  of the module M, unless M is a submodule of D^r, in which
--	  case a resolution of D^r/M is returned",
--	  "Dresolution (Ideal, List) -- returns a w-adapted resolution of D/I,
--	  where w is a weight vector of the form (-u,u)",
--	  "Dresolution (Module, List) -- returns a w-adapted resolution
--	  of the module M, unless M is a submodule of D^r, in which
--	  case a resolution of D^r/M is returned"},
    
--     "See also :",     
--     MENU{HREF{"/HOME/Drestriction.html","Drestriction"}}
     }
