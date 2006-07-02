document { Key => {(Grassmannian, ZZ, ZZ), Grassmannian},
     Headline => "the Grassmannian of linear subspaces of a vector space",
     Usage => "Grassmannian(k,r)",
     Inputs => { 
	  "k", 
	  "r",
	  CoefficientRing => Ring => "the coefficient ring for the polynomial ring to be made",
	  Variable => Symbol => { "the base symbol for the indexed variables to be used.  The subscripts are the elements of ", TT "subsets(n+1,k+1)" }
	  },
     Outputs => {{ "the ideal of the Grassmannian variety of all projective ", TT "k", "-planes in ", BOLD "P", SUP "r"}},
     EXAMPLE lines ///
	 Grassmannian(1,3)
	 J = Grassmannian(2,5, CoefficientRing => ZZ/31, Variable => T)
     ///,
     "The variables of the ring are based on the symbol provided, but assignments
     are not made until the ring or the ideal is assigned to a global variable or is submitted to ", TO "use", ", as follows.",
     EXAMPLE {
	  "T_(0,2,3)",
	  "use ring J",
	  "T_(0,2,3)"
	  },
     "In many ways, more natural than returning an ideal would be to return the corresponding quotient ring or
     variety, but creating a quotient ring involves computing a Groebner basis, which
     might impose a heavy computational burden that the user would prefer to avoid.",
     SeeAlso => {"Schubert","pfaffians"}
     }

document { 
     Key => {(Schubert,ZZ,ZZ,VisibleList), Schubert},
     Headline => "find the Pluecker ideal of a Schubert variety",
     Usage => "Schubert(k,n,sigma)",
     Inputs => { 
	  "k", 
	  "n", 
	  "sigma" => {"a subset of ", TT "0..n", " of size ", TT "k+1", " that indexes the Schubert variety"},
	  CoefficientRing => Ring => "the coefficient ring for the polynomial ring to be made",
	  Variable => Symbol => { "the base symbol for the indexed variables to be used.  The subscripts are the elements of ", TT "subsets(n+1,k+1)" }
	  },
     Outputs => {Ideal => "the ideal of the Schubert variety indexed by sigma" },
     TEX ///Given natural numbers $k \le{} n$, this routine finds the
     ideal of the Schubert variety indexed by sigma in the Grassmannian of projective 
     $k$-planes in $P^n$.///,
     PARA {
     	  "For example, the ideal of the Schubert variety indexed by ", TT "(1,3,4)", 
     	  " in the Grassmannian of projective planes in ", TT "P^5", " is displayed in the following example."
     	  },
     EXAMPLE lines ///
	  I = Schubert(2,4,{1,2,4})
	  R = ring I;
	  C = res I
	  betti C
     ///,
     SeeAlso => {"Grassmannian","pfaffians"}
     }
