needs "D-modules.m2"

document { DHom,
     TT "DHom (M, N)", " -- computes a basis of
     D-homomorphisms between holonomic D-modules M and N.",
     BR, NOINDENT,
     TT "DHom (I, J)", " -- computes a basis of
     D-homomorphisms between D/I and D/J.",

     PARA,
     "The set of D-homomorphisms between two holonomic modules M and N
     is a finite-dimensional vector space over the ground field.
     Since a homomorphism is defined by where it sends a set of generators,
     the output of this command is a list of matrices whose columns
     correspond to the images of the generators of M.  Here
     the generators of M are determined from its presentation
     by generators and relations.",

     PARA,
     "The algorithm used appears in the paper 'Computing homomorphisms
     between holonomic D-modules' by Tsai-Walther(2000).
     The method is to combine isomorphisms of Bjork and Kashiwara with
     the restriction algorithm.",

     PARA,
     "A simple example:",
     EXAMPLE {
	  ///needs "D-modules.m2"///,
	  "W = QQ[x, D, WeylAlgebra=>{x=>D}]",
      	  "M = W^1/ideal(D-1)",
      	  "N = W^1/ideal((D-1)^2)",
      	  "DHom(M,N)"},

     PARA,
     "Optional arguments :",     
     MENU{"DHom (..., Strategy => Schreyer,...) -- option passed to Dresolution",
	  "DHom (..., Strategy => Vhomogenize,...) -- option passed to Dresolution"},

     "Caveats and known problems :",     
     MENU{"Input modules M, N, D/I, or D/J should be holonomic."},

     SEEALSO {"DExt", "Drestriction"}

--     "Ways to use ", TO "DHom",
--     MENU {"DHom (Ideal, Ideal) -- returns a basis of homomorphisms between
--	  holonomic D/I and D/J",
--	  "DHom (Module, Module) -- returns a basis of homomorphisms between
--	  holonomic M and N"},
     
--     "See also :",     
--     MENU{HREF{"/HOME/DExt.html","DExt"},
--	  HREF{"/HOME/PolySols.html","PolySols"},
--	  HREF{"/HOME/RatSols.html", "RatSols"},
--	  HREF{"/HOME/Drestriction.html","Drestriction"}}
     },

document { DExt,
     --BIG "Drestriction",
     TT "DExt (M, N)", " -- 
     computes the dimensions of the Ext groups between
     holonomic M and holonomic N.",
     BR, NOINDENT,
     TT "DExt (I, J)", " -- 
     computes the dimensions of the Ext groups between
     holonomic D/I and holonomic D/J.",

     PARA,
     "The Ext groups between D-modules M and N
     are the derived functors of Hom, and are finite-dimensional
     vector spaces over the ground field when M and N are holonomic.",

     PARA,
     "The algorithm used appears in the paper 'Polynomial and
     rational solutions of holonomic systems' by Oaku-Takayama-Tsai (2000).
     The method is to combine isomorphisms of Bjork and Kashiwara with
     the restriction algorithm.",

     PARA,
     "A simple example:",
     EXAMPLE {
	  ///needs "D-modules.m2"///,
	  "W = QQ[x, D, WeylAlgebra=>{x=>D}]",
      	  "M = W^1/ideal(x*(D-1))",
      	  "N = W^1/ideal((D-1)^2)",
      	  "DExt(M,N)"},

     PARA,
     PARA,
     "Optional arguments :",     
     MENU{"DExt (..., Strategy => Schreyer,...) -- option passed to Dresolution",
	  "DExt (..., Strategy => Vhomogenize,...) -- option passed to Dresolution"},

     "Caveats and known problems :",     
     MENU{"Input modules M, N, D/I, or D/J should be holonomic.",
	  "Does not yet compute explicit reprentations of Ext groups
	  such as Yoneda representation."},

     SEEALSO {"DHom", "Drestriction"}

--     "Ways to use ", TO "DExt",
--     MENU {"DHom (Ideal, Ideal) -- returns dimensions of the Ext groups
--	  between holonomic D/I and D/J",
--	  "DHom (Module, Module) -- returns dimensions of the Ext groups
--	  between holonomic M and N"},
     
--     "See also :",     
--     MENU{HREF{"/HOME/DHom.html","DHom"},
--	  HREF{"/HOME/PolyExt.html","PolyExt"},
--	  HREF{"/HOME/RatExt.html", "RatExt"},
--	  HREF{"/HOME/Drestriction.html","Drestriction"}}
     }

document { PolySols,
     TT "PolySols I", " -- computes a basis of the polynomial solutions
     of a holonomic ideal I",
     BR, NOINDENT,
     TT "PolySols M", " -- computes a basis of D-homomorphisms
     between a holonomic module M and the polynomial ring",
     BR, NOINDENT,
     TT "PolySols (I, w)", " -- computes a basis of polynomial solutions
     of I using the weight vector w for Grobner deformations",
     BR, NOINDENT,
     TT "PolySols (M, w)", " -- computes a basis of D-homomorhpisms between
     M and the polynomial ring using w for Grobner deformations",

     PARA,
     "The polynomial solutions of a holonomic system form a
     finite-dimensional vector space.
     There are two algorithms implemented to get these solutions.
     The first algorithm is based on Grobner deformations and
     works for ideals I of PDE's -- see the paper 'Polynomial
     and rational solutions of a holonomic system' by 
     Oaku-Takayama-Tsai (2000).  The second algorithm is based
     on homological algebra -- see the paper 'Computing
     homomorphims between holonomic D-modules' by Tsai-Walther (2000).",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "W = QQ[x, D, WeylAlgebra=>{x=>D}]",
     EXAMPLE "I = ideal(D^2, (x-1)*D-1)",
     EXAMPLE "PolySols I",

     PARA,
     "Optional arguments :",     
     MENU{"PolySols (..., Algorithm => GD,...) -- uses Grobner deformations",
	  "PolySols (..., Algorithm => Duality,...) -- uses homological
	  duality"},

     SEEALSO {"RatSols", "Dintegration"}

--     "Ways to use ", TO "PolySols",
--     MENU {"PolySols (Ideal) -- returns a basis of polynomial
--	  solutions of the holonomic ideal I",
--	  "PolySols (Module) -- returns a basis of homomorphisms between
--	  holonomic M and the polynomial ring",
--	  "PolySols (Ideal, List) -- returns a basis of polynomial
--	  solutions to I.  Uses weight vector w for Grobner deformations
--	  or V-strict resolution",
--	  "PolySols (Module, List) -- returns a basis of homomorphisms between
--	  holonomic M and the polynomial ring.  Uses weight vector w for 
--	  Grobner deformations or V-strict resolution"},
     
--     "See also :",   
--     MENU{HREF{"/HOME/PolyExt.html","PolyExt"},
--	  HREF{"/HOME/RatSols.html", "RatSols"},
--	  HREF{"/HOME/DHom.html","DHom"}}     
     },

document { PolyExt,
     TT "PolyExt M", " -- 
     computes the dimensions of the Ext groups between
     holonomic M and the polynomial ring.",
     BR, NOINDENT,
     TT "PolyExt I", " -- 
     computes the dimensions of the Ext groups between
     holonomic D/I and the polynomial ring.",

     PARA,
     "The Ext groups between a D-module M and the polynomial ring
     are the derived functors of Hom, and are finite-dimensional
     vector spaces over the ground field when M is holonomic.",

     PARA,
     "The algorithm used appears in the paper 'Polynomial and
     rational solutions of holonomic systems' by Oaku-Takayama-Tsai (2000).
     The method is to combine isomorphisms of Bjork and Kashiwara with
     the restriction algorithm.",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "W = QQ[x, D, WeylAlgebra=>{x=>D}]",
     EXAMPLE "M = W^1/ideal(x^2*D^2)",
     EXAMPLE "PolyExt(M)",

     PARA,
     "Optional arguments :",
     MENU{"PolySols (..., Strategy => Schreyer,...) -- option passed to Dresolution",
	  "PolySols (..., Strategy => Vhomogenize,...) -- option passed to Dresolution"},

     "Caveats and known problems :",     
     MENU{"Does not yet compute explicit representations of
	  Ext groups such as Yoneda representation."},

     SEEALSO {"PolySols", "RatExt", "DExt", "Dintegration"}

--     "Ways to use ", TO "PolyExt",
--     MENU {"PolySols (Ideal) -- returns dimensions of the Ext groups
--	  between holonomic D/I and the polynomial ring",
--	  "PolySols (Module) -- returns dimensions of the Ext groups
--	  between holonomic M and the polynomial ring"},
    
--     "See also :", 
--     MENU{HREF{"/HOME/PolySols.html","PolySols"},
--	  HREF{"/HOME/RatExt.html","RatExt"},
--	  HREF{"/HOME/DExt.html", "DExt"},
--	  HREF{"/HOME/Drestriction.html","Drestriction"}}     
     }

document { RatSols,
     TT "RatSols I", " -- computes a basis of the rational solutions
     of a holonomic ideal I",
     BR, NOINDENT,
     TT "RatSols (I, f)", " -- computes a basis of rational solutions to I
     with poles along f",
     BR, NOINDENT,
     TT "RatSols (I, f, w)", " -- computes a basis of rational solutions
     to I with poles along f
     using the weight vector w for Grobner deformations",
     BR, NOINDENT,
     TT "PolySols (I, ff, w)", " -- computes a basis of rational sollutions
     with poles along the polynomials in the list ff 
     using w for Grobner deformations",

     PARA,
     "The rational solutions of a holonomic system form a
     finite-dimensional vector space.
     The only possibilities for the poles of a rational solution
     are the codimension one components of the singular locus.
     An algorithm to compute rational solutions 
     is based on Grobner deformations and
     works for ideals I of PDE's -- see the paper 'Polynomial
     and rational solutions of a holonomic system' by 
     Oaku-Takayama-Tsai (2000).",
     
     PARA,
     "A simple example:",
     EXAMPLE {
	  ///needs "D-modules.m2"///,
      	  "W = QQ[x, D, WeylAlgebra=>{x=>D}]",
      	  "I = ideal((x+1)*D+5)",
      	  "RatSols I"
	  },

     PARA,
     "Caveats and known problems :",     
     MENU{"The most efficient method to find rational solutions is
	  to find the singular locus, then try to find its irreducible
	  factors.  With these, call RatSols(I, ff, w), where w
	  should be generic enough so that the PolySols routine
	  will not complain of a non-generic weight vector."},
     
     SEEALSO {"PolySols", "RatExt", "DHom"} 
     },

document { RatExt,
     --BIG "Drestriction",
     TT "RatExt M", "-- 
     computes the dimensions of the Ext groups between
     holonomic M and the polynomial ring localized at 
     the singular locus of M.",
     BR, NOINDENT,
     TT "RatExt I", "-- 
     computes the dimensions of the Ext groups between
     D/I and the polynomial ring localized at 
     the singular locus of D/I.",
     BR, NOINDENT,
     TT "RatExt (M, f)", "-- 
     computes the dimensions of the Ext groups between
     M and the polynomial ring localized at f.",
     BR, NOINDENT,
     TT "RatExt (I, f)", "-- 
     computes the dimensions of the Ext groups between
     D/I and the polynomial ring localized at f.",

     PARA,
     "The Ext groups between M and N
     are the derived functors of Hom, and are finite-dimensional
     vector spaces over the ground field when M and N are holonomic.",

     PARA,
     "The algorithm used appears in the paper 'Polynomial and
     rational solutions of holonomic systems' by Oaku-Takayama-Tsai (2000).
     The method is to combine isomorphisms of Bjork and Kashiwara with
     the restriction algorithm.",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "W = QQ[x, D, WeylAlgebra=>{x=>D}]",
     EXAMPLE "M = W^1/ideal(x*D+5)",
     EXAMPLE "RatExt(M)",

     PARA,
     "Optional arguments :",     
     MENU{"RatExt (..., Strategy => Schreyer,...) -- option passed to Dresolution",
	  "RatExt (..., Strategy => Vhomogenize,...) -- option passed to Dresolution"},

     "Caveats and known problems :",     
     MENU{"Input modules M or D/I should be holonomic."},

     SEEALSO {"Dresolution", "Dintegration"}

--     "Ways to use ", TO "RatExt",
--     MENU {"RatSols (Ideal) -- returns dimensions of the Ext groups
--	  between holonomic D/I and the localization of the
--	  polynomial ring at the singular locus of I",
--	  "RatSols (Module) -- returns dimensions of the Ext groups
--	  between holonomic M and the localization of the
--	  polynomial ring at the singular locus of M",
--	  "RatSols (Ideal, RingElement) -- returns dimensions of the 
--	  Ext groups between holonomic D/I and the localization of the
--	  polynomial ring at f",
--	  "RatSols (Module, RingElement) -- returns dimensions of the 
--	  Ext groups between holonomic M and the localization of the
--	  polynomial ring at f"},
     
--     "See also :",     
--     MENU{HREF{"/HOME/RatSols.html","RatSols"},
--	  HREF{"/HOME/PolyExt.html","PolyExt"},
--	  HREF{"/HOME/DExt.html", "DExt"},
--	  HREF{"/HOME/Drestriction.html","Drestriction"}}
     }

