needs "D-modules.m2"

document { deRham,
     TT "deRham f", "-- 
     computes the deRham cohomology groups of the complement of the
     hypersurface {f = 0} ",
     BR, NOINDENT,
     TT "deRham (i, f)", "-- 
     computes i-th deRham cohomology group of the complement of the
     hypersurface {f = 0} ",
     BR, NOINDENT,
     TT "deRhamAll f", "-- 
     returns explicit cohomology classes in the deRham complex and
     supplementary information",
	  
     PARA,
     "The algorithm used appears in the paper 'An algorithm for deRham 
     cohomology groups of the complement of an affine variety via D-module
     computation' by Oaku-Takayama(1999).  
     The method is to compute the localization of the polynomial ring 
     by f, then compute the derived integration of the localization.
     The routine deRhamAll can be used to compute cup product structures
     as in the paper 'The cup product structure for complements
     of affine varities' by Walther(2000).",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "R = QQ[x,y]",
     EXAMPLE "f = x^2-y^3", 
     EXAMPLE "deRham(f)",

     PARA,
     "Optional arguments :",
     MENU{"deRham (..., Strategy => Schreyer,...) -- option passed to Dresolution",
	  "deRham (..., Strategy => Vhomogenize,...) -- option passed to Dresolution"},

     SEEALSO {"Dlocalization", "Dintegration"}

--     "Ways to use ", TO "deRham",
--     MENU {"deRham (RingElement) -- returns hash table of the deRham
--	  cohomology groups of the complement of {f=0}", 
--	  "deRham (ZZ, RingElement) -- 
--	  returns i-th deRham cohomology group",  "deRhamAll (RingElement) --
-- 	  returns explicit cohomology classes in the deRham complex and
--	  supplementary information.  Can be used
--	  to then compute the cup product structure by the algorithm
--	  in 'The cup product structure for complements of complex
--	  affine varieties' by Walther(1999)."},

--     "See also:",     
--     MENU{HREF{"/HOME/Dintegration.html","Dintegration"}}

     }
