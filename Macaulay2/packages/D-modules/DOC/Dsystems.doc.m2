needs "D-modules.m2"

document { gkz,
     TT "gkz (A,b)", " -- 
     computes the Gel'fand-Kapranov-Zelevinsky hypergeometric ideal
     associated to the matrix A and parameter b",
     BR, NOINDENT,
     TT "gkz (A)", " -- 
     computes parametric Gel'fand-Kapranov-Zelevinsky hypergeometric ideal
     associated to the matrix A",

     PARA,
     "The GKZ hypergeometric system of PDE's associated to a (d x n)
     integer matrix A consists of the toric ideal I_A in the polynomial
     subring C[d_1,...,d_n] and Euler relations given by the entries
     of the vector (A theta - b), where theta is the vector
     (theta_1,...,theta_n)^t, and theta_i = x_i d_i.
     See the book 'Grobner deformations of hypergeometric differential 
     equations' by Saito-Sturmfels-Takayama (1999) for more details.",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "A = matrix{{1,1,1},{0,1,2}}",
     EXAMPLE "b = {3,4}", 
     EXAMPLE "I = gkz (A,b)",

     "Caveats and known problems :",
     MENU{"gkz always returns a different ring and will use variables
	  x_1,...,x_n, D_1,...D_n."},

     SEEALSO {"AppellF1"}

--     PARA,
--     "Ways to use ", TO "gkz",
--     MENU {"gkz (Matrix, List) -- returns the gkz ideal associated to
--	  the matrix A and parameter vector b", 
--	  "gkz (Matrix) -- returns the gkz ideal associated
--	  to the matrix A with generic parameters"},

--     "See also:",     
--     MENU{HREF{"/HOME/AppellF1.html","AppellF1"}}
     },

document { AppellF1,
     TT "AppellF1 {a0,a1,a2,a3}", " -- 
     compute the Appell F1 system of PDE's associated to the
     parameters a0, a1, a2, and a3.",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "w = {1,4/5,-2,3/2}",
     EXAMPLE "I = AppellF1(w)",

     PARA,
     "Caveats and known problems :",     
     MENU{"AppellF1 always returns a different ring and will
	  use variables x and y. Input should be a List of 4
	  numbers."},

     SEEALSO {"gkz"}

--     "Ways to use ", TO "AppellF1",
--     MENU {"AppellF1 (List) -- returns the Appell F1 system
--	  associated to the parameter vector w, which is a
--	  list of 4 numbers."},

--     "See also:",
--     MENU{HREF{"/HOME/gkz.html","gkz"}}
     }

document { PolyAnn,
     TT "PolyAnn f", " -- 
     compute the annihilator ideal in the Weyl algebra of the polynomial f",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx, y=>Dy}]",
     EXAMPLE "f = x^2-y^3",
     EXAMPLE "I = PolyAnn f",

     PARA,
     "Caveats and known problems :",     
     MENU{"The input f should be an element of a Weyl algebra,
	  and not an element of a commutative polynomial ring.
	  However, f should only involve commutative variables."},

     SEEALSO {"RatAnn"}

--     "Ways to use ", TO "PolyAnn",
--     MENU {"PolyAnn (RingElement) -- returns the annihilator of
--	  a polynomial f.  The input f should be an element in
--	  the coordinate variables (as opposed to derivative variables)
--	  of a Weyl algebra."},

--     "See also:",   
--     MENU{HREF{"/HOME/RatAnn.html","RatAnn"}}
     }

document { RatAnn,
     TT "RatAnn f", " -- 
     compute the annihilator ideal in the Weyl algebra of the rational
     function 1/f",
     BR, NOINDENT,
     TT "RatAnn (g,f)", " -- 
     compute the annihilator ideal in the Weyl algebra of the rational
     function g/f",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx, y=>Dy}]",
     EXAMPLE "f = x^2-y^3",
     EXAMPLE "g = 2*x*y",
     EXAMPLE "I = RatAnn (g,f)",

     PARA,
     "Caveats and known problems :",
     MENU{"The inputs f and g should be elements of a Weyl algebra,
	  and not elements of a commutative polynomial ring.
	  However, f and g should only use the commutative variables."},

     SEEALSO {"PolyAnn"}

--     "Ways to use ", TO "RatAnn",
--     MENU {"PolyAnn (RingElement, RingElement) -- returns the ideal of operators
--	  in the Weyl algebra which annihilate
--	  the rational function g/f.  The inputs f and g should be an element in
--	  the coordinate variables (as opposed to derivative variables)
--	  of a Weyl algebra.",
--	  "RatAnn (RingElement) -- returns the annihilating ideal in the
--	  Weyl algebra of the rational function 1/f."},

--     "See also:",     
--     MENU{HREF{"/HOME/PolyAnn.html","PolyAnn"}}
     }
