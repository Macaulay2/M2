needs "D-modules.m2"

document { inw,
     TT "inw (L, w)", " -- 
     computes the initial form of an element L
     with respect to a weight vector w.",
     BR, NOINDENT,
     TT "inw (I, w)", " -- 
     computes the initial ideal of an ideal I
     with respect to a weight vector w.",
     BR, NOINDENT,
     TT "inw (m, w)", " -- 
     computes the initial matrix of a matrix m
     with respect to a weight vector w.",

     PARA,
     "This routine computes the initial ideal of a left ideal I 
     of the Weyl algebra with respect to a weight vector w = (u,v)
     where u+v >= 0.
     In the case where u+v > 0, then the ideal lives in the 
     associated graded ring which is a commutative ring.  In the case
     where u+v = 0, then the ideal lives in the associated graded
     ring which is again the Weyl algebra.  In the general case u+v >= 0,
     the associated graded ring is somewhere between.  There are
     two strategies to compute the initial ideal.  One is to homogenize
     to an ideal of the homogeneous Weyl algebra.  The other is
     to homogenize with respect to the weight vector w.  These strategies
     can be used by toggling HOMOGENIZATION",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]",
     EXAMPLE "I = ideal (x*Dx+2*y*Dy-3, Dx^2-Dy)", 
     EXAMPLE "inw(I, {1,3,3,-1})",
     EXAMPLE "inw(I, {-1,-3,1,3})",

     PARA,
     "Caveats and known problems :",
     MENU{"The weight vector w = (u,v) must have u+v>=0."},

     SEEALSO {"gbw"}

--     "Ways to use ", TO "inw",
--     MENU {"inw (Ideal, List) -- returns the initial ideal
--	  of an ideal I of the Weyl algebra with respect to the
--	  weight vector w",
--	  "inw (Matrix, List) -- returns the initial ideal
--	  of a matrix m of the Weyl algebra with respect to the
--	  weight vector w"},
--     "Strategies :",
--     MENU{"inw(... Strategy => VHom) -- uses homogeneous Weyl algebra 
--	  method of Takayama",
--	  "inw(... Strategy => VHom) -- uses V-homogenization method of Oaku"},
--     "See also :",     
--     MENU{HREF{"/HOME/gbw.html","gbw"}}
     },

document { gbw,
     TT "gbw (I, w)", " -- 
     computes a Grobner basis of an ideal with respect
     to a weight vector w.",
     BR, NOINDENT, TT "gbw (m, w)", " -- 
     computes a Grobner basis of a matrix with respect
     to a weight vector w.",

     PARA,
     "This routine computes a Grobner basis of a left ideal I 
     of the Weyl algebra with respect to a weight vector w = (u,v)
     where either u+v > 0 or u+v = 0.  In the case where u+v > 0,
     then the ordinary Buchberger algorithm works for any term order
     refining the weight order. In the case
     where u+v = 0, then the Buchberger algorithm needs to be adapted to
     guarantee termination.  There are two strategies to do this.  
     One is to homogenize
     to an ideal of the homogeneous Weyl algebra.  The other is
     to homogenize with respect to the weight vector w.",

     PARA,
     "A simple example:",
     EXAMPLE ///needs "D-modules.m2"///,
     EXAMPLE "W = QQ[x,y,Dx,Dy, WeylAlgebra => {x=>Dx,y=>Dy}]",
     EXAMPLE "I = ideal (x*Dx+2*y*Dy-3, Dx^2-Dy)", 
     EXAMPLE "gbw(I, {1,3,3,-1})",
     EXAMPLE "gbw(I, {-1,-3,1,3})",

     PARA,
     "Caveats and known problems :",
     
     MENU{"The weight vector w = (u,v) must have u+v>=0."},

     SEEALSO {"inw"}


--     "Ways to use ", TO "gbw",
--     MENU {"gbw (Ideal, List) -- returns a Grobner basis
--	  of the ideal I of the Weyl algebra with respect to the
--	  weight vector w",
--	  "gbw (Matrix, List) -- returns a Grobner basis
--	  of a matrix m of the Weyl algebra with respect to the
--	  weight vector w"},

--     "Strategies :",
--     MENU{"gbw(... Strategy => VHom) -- uses homogeneous Weyl algebra 
--	  method of Takayama",
--	  "gbw(... Strategy => VHom) -- uses V-homogenization method of Oaku"},

--     "See also :",
--     MENU{HREF{"/HOME/inw.html","inw"}}
     }

