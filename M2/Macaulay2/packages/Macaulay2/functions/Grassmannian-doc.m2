--- status: DRAFT
--- author(s): 
--- notes: GRASSMANNIAN REWRITTEN 

document { 
     Key => {Grass, (Grass,ZZ,ZZ)
       	  },
     Headline => "find the Pluecker ideal of a Grassmannian",
     Usage => {"Grass(k,n)"},
     Inputs => {"k" => ZZ => "","n" => ZZ => ""  
	  },
     Outputs => {Ideal => "the ideal of the Grassmannian"
	  },
     PARA {
	  TEX "Given natural numbers $k \\le{} n$, the routine finds the ideal 
	  of the Grassmannian of projective $k$-planes in $P^n$, in the ring ",
	  TT "ZZ[apply(toSequence \\ subsets(n+1,k+1), i->p_i)]", "."
	  },
     PARA {
	  TEX "For example, the ideal of the Grassmannian of projective lines in $P^3$ is:"
	  },
     EXAMPLE {
	  "I = Grass(1,3)"
     	  },
     PARA {
	  TEX "The ideal of the Grassmannian of projective planes in $P^4$ is"
	  },
     EXAMPLE{ 
          "J = Grass(2,4)"
     	  },
     "The variables of the ring are based on the symbol ", TT "p", ", but assignments
     are not made until the ring or the ideal is submitted to ", TO "use", ", as follows.",
     EXAMPLE {
	  "p_(0,2,3)",
	  "use J",
	  "p_(0,2,3)"
	  },
     "In many ways, more natural than returning an ideal would be to return the corresponding quotient ring or
     variety, but creating a quotient ring involves computing a Groebner basis, which
     might impose a heavy computational burden that the user would prefer to avoid.",
     SeeAlso => {"Schubert","pfaffians"}
     }

document { 
     Key => {(Schubert,ZZ,ZZ,List), Schubert},
     Headline => "find the Pluecker ideal of a Schubert variety",
     Usage => "Schubert(k,n,sigma)",
     Inputs => { "k" => "", "n" => "", "sigma" => {"a subset of ", TT "0..n", " of size ", TT "k+1", " that indexes the Schubert variety"} },
     Outputs => {Ideal => "the ideal of the Schubert variety indexed by sigma" },
     TEX ///Given natural numbers $k \le{} n$, this routine finds the
     ideal of the Schubert variety indexed by sigma in the Grassmannian of projective 
     $k$-planes in $P^n$.///,
     PARA {
     	  "For example, the ideal of the Schubert variety indexed by ", TT "(1,3,4)", 
     	  " in the Grassmannian of projective planes in ", TT "P^5", " is displayed in the following example."
     	  },
     EXAMPLE {
	  "I = Schubert(2,4,{1,2,4})",
	  "R = ring I;",
	  "C = res I",
	  "betti C"
     	  },
     SeeAlso => {"Grass","pfaffians"}
     }
    
-- doc11.m2:832:     Key => Grassmannian,
--  Description => {TT "Grassmannian(k,r)", " -- Grassmannian of k-planes in P^r",BR,NOINDENT,
--     TT "Grassmanian(k,r,R)", 
--     PARA,
--     "Given natural numbers k <= r, and optionally a ring ", TT "R", " with
--     at least ", TT "binomial(r+1,k+1)", " variables, the routine finds the
--    ideal of the Grassmannian of projective k-planes in P^r, using the
--     first ", TT "binomial(r+1,k+1)", " variables of ", TT "R", ".  If ", TT "R", "
--     is not given, the routine makes and uses ", TT "ZZ/31991[vars(0..binomial(r+1,k+1)-1].",
--     PARA,
--	  
--	  }
--    "For example, the Grassmannian of projective lines in ", TT "P^3", ":",
--     EXAMPLE {"J = Grassmannian(1,3)",
--              "R = QQ[a..f];",
--              "J = Grassmannian(1,3,R)",
--	  },
--     Caveat => {"Currently, this ideal is constructed using relations on minors
--     of a generic matrix.  It should really use the Pluecker equations."
--     },