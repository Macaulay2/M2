needs "D-modules.m2"

document { Dlocalize,
     TT "Dlocalize (M, f)", " -- 
     compute the localization of the D-module M with respect to the
     polynomial f",
     BR, NOINDENT,
     TT "Dlocalize (I, f)", " -- 
     compute the localization of the quotient module D/I with respect to the
     polynomial f",
     PARA,
     TT "DlocalizeMap (M, f)", " -- 
     compute the localization map M --> M[1/f]",
     BR, NOINDENT,
     TT "DlocalizeMap (M, f)", " -- 
     compute the localization map (D/I) --> (D/I)[1/f]",
     PARA,
     TT "DlocalizeAll (M, f)", " -- 
     compute the localization of M with respect to f and
     some auxilary information",
     BR, NOINDENT,
     TT "DlocalizeAll (M, f)", " -- 
     compute the localization of D/I with respect to f and some
     auxilary information",

     PARA,
     "One of the nice things about D-modules is that if a finitely
     generated D-module is specializable along f, then it's localization 
     with respect to f is also finitely generated.  For instance,
     this is true for all holonomic D-modules.",
     
     PARA,
     "There are two different algorithms for localization implemented.  
     The first appears in the
     paper 'A localization algorithm for D-modules' by Oaku-Takayama-Walther
     (1999).  The second is due to Oaku and appears in the paper
     'Algorithmic computation of local cohomology modules and the
     cohomological dimension of algebraic varieties' by Walther(1999)",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]",
     EXAMPLE "M = W^1/(ideal(x*Dx+1, Dy))", 
     EXAMPLE "f = x^2-y^3",
     EXAMPLE "Mf = Dlocalize(M, f)",

     PARA,
     "Optional arguments :",     
     MENU{"Dlocalize (..., Strategy => Oaku,...) -- use the Oaku algorithm",
	  "Dlocalize (..., Strategy => OTW,...) -- use the 
	  Oaku-Takayama-Walther algorithm"},

     PARA,
     "Abbreviations :",
     MENU{"Dlocalization"},
     
     SEEALSO {"AnnFs", "Dintegration"}

--     "Ways to use ", TO "Dlocalize",
--     MENU {"Dlocalize (Ideal, RingElement) -- returns the localization
--	  of D/I by f", "Dlocalize (Module, RingElement) -- returns
--	  the localization of M by f", "DlocalizeMap (Ideal, RingElement) -- 
--	  returns a map from D/I to the localization of D/I by f",
--	  "DlocalizeMap (Module, RingElement) -- returns a map from
--	  M to the localization of M by f",
--	  "DlocalizeAll (Ideal, RingElement) -- returns hash table
--	  containing localization of D/I, localization map, and other
--	  extra information,", 
--	  "DlocalizeAll (Ideal, RingElement) -- returns hash table
--	  containing localization of M, localization map, and other
--	  extra information"},

--     "See also:",     
--     MENU{HREF{"/HOME/Drestriction.html","Drestriction"}}
     }
