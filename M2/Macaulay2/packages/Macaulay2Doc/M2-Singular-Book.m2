document { 
	Key => "M2SingularBook",
    	Headline => "Macaulay2 examples for the Singular book",
    	EM "M2SingularBook", " consists of Macaulay2 translations of the examples in the book ",
	      EM "A Singular introduction to commutative algebra",
	PARA{},
	"Each example is numbered as it is in Greuel and Pfister's book.  Links to 
	Macaulay2 routines and concepts are also provided. ",
	PARA{},
	-- Mike wanted this: TO "Macaulay2 implementation bugs and problems",
	Subnodes => {
	     "Chapter 1",
	     TO "Singular Book 1.1.8",
	     TO "Singular Book 1.1.9",
	     TO "Singular Book 1.1.10",
	     TO "Singular Book 1.2.3",
	     TO "Singular Book 1.2.13",
	     TO "Singular Book 1.3.3",
	     TO "Singular Book 1.3.13",
	     TO "Singular Book 1.3.15",
	     TO "Singular Book 1.4.9",
	     TO "Singular Book 1.5.10",
	     TO "Singular Book 1.6.13",
	     TO "Singular Book 1.7.10",
	     TO "Singular Book 1.7.12",
	     TO "Singular Book 1.8.1",
	     TO "Singular Book 1.8.2",
	     TO "Singular Book 1.8.4",
	     TO "Singular Book 1.8.6",
	     TO "Singular Book 1.8.7",
	     TO "Singular Book 1.8.9",
	     TO "Singular Book 1.8.11",
	     TO "Singular Book 1.8.13",
	     TO "Singular Book 1.8.15",
	     TO "Singular Book 1.8.18",
	     TO "Singular Book 1.8.19",
	     TO "Singular Book 1.8.20",
	     "Chapter 2",
	     TO "Singular Book 2.1.6",
	     TO "Singular Book 2.1.7",
	     TO "Singular Book 2.1.10",
	     TO "Singular Book 2.1.13",
	     TO "Singular Book 2.1.20",
	     TO "Singular Book 2.1.24",
	     TO "Singular Book 2.1.26",
	     TO "Singular Book 2.1.34",
	     TO "Singular Book 2.2.15",
	     TO "Singular Book 2.3.10",
	     TO "Singular Book 2.3.12",
	     TO "Singular Book 2.4.12",
	     TO "Singular Book 2.4.15",
	     TO "Singular Book 2.5.5",
	     TO "Singular Book 2.5.18",
	     TO "Singular Book 2.6.3",
	     TO "Singular Book 2.6.11",
	     TO "Singular Book 2.6.15",
	     TO "Singular Book 2.7.5",
	     TO "Singular Book 2.7.9",
	     TO "Singular Book 2.7.14",
	     TO "Singular Book 2.8.1",
	     TO "Singular Book 2.8.3",
	     TO "Singular Book 2.8.5",
	     TO "Singular Book 2.8.6",
	     TO "Singular Book 2.8.7",
	     TO "Singular Book 2.8.8",
	     TO "Singular Book 2.8.9",
	     TO "Singular Book 2.8.10",
	     }
	}
-- document {
--      Key => "Macaulay2 implementation bugs and problems",
--      UL {
-- 	  "Some monomial orders should be easier to specify",
-- 	  "Local remainder is wrong",
-- 	  "Local GB can be improved: x13+x14y can be reduced??",
-- 	  "Highest corner use in local computations can speed things up...",
-- 	  "1.1.8: RR and CC: how to use them!?",
-- 	  "1.8.2: how are we doing local division?",
-- 	  "1.8.2: write this node",
-- 	  "1.8.7: solving equations is not quite implemented.  
-- 	    But it should not be that hard...",
-- 	  "1.8.15: implement sat, saturation which returns (Isat,ZZ).",
-- 	  "1.8.18, 1.8.19: write these",
-- 	  "2.1.34: prune for local rings is not yet available",
-- 	  }
--      }
document {
     Key => "Singular Book 1.1.8",
     Headline => "computation in fields",
     SUBSECTION "Computation over ZZ and QQ",
     "In Macaulay2, Integers are arbitrary precision.  The ring of integers is denoted ZZ.",
     EXAMPLE {
	  "123456789^5",
	  "matrix{{123456789^5}}",
	  "gcd(3782621293644611237896400,85946734897630958700)"
	  },
     "The ring of rational numbers is denoted by QQ.",
     EXAMPLE {
	  "n = 12345/6789",
	  "n^5",
	  "toString(n^5)"
	  },
     SUBSECTION "Computation in finite fields",
     EXAMPLE {
	  "A = ZZ/32003;"
	  },
     "In order to do arithmetic in this ring, you must construct elements of this ring.
     ", TT "n_A", " gives the image of the integer n in A.",
     EXAMPLE {
	  "123456789 * 1_A",
	  "(123456789_A)^5"
	  },
     EXAMPLE {
	  "A2 = GF(8,Variable=>a)",
	  "ambient A2",
	  "a^3+a+1"
	  },
     EXAMPLE {
	  "A3 = ZZ/2[a]/(a^20+a^3+1);",
	  "n = a+a^2",
	  "n^5"
  	  },
     SUBSECTION "Computing with real and complex numbers",
     EXAMPLE {
	  "n = 123456789.0",
	  "n = n * 1_RR",
	  "n^5",
	  },
     SUBSECTION "Computing with parameters",
     EXAMPLE {
	  "R3 = frac(ZZ[a,b,c])",
	  "n = 12345*a + 12345/(78*b*c)",
	  "n^2",
	  "n/(9*c)"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.1.9",
     Headline => "computation in polynomial rings",
     "Create a polynomial ring using reasonably standard notation.",
     EXAMPLE {
	  "A = QQ[x,y,z];",
	  "f = x^3+y^2+z^2",
	  "f^2-f"
	  },
     "Here are several more examples.",
     EXAMPLE {
	  "B = ZZ/32003[x,y,z];",
	  "C = GF(8)[x,y,z];",
	  "D = ZZ[x,y,z];",
	  "E = (frac(ZZ[a,b,c]))[x,y,z];"
	  },
     "In Macaulay2, there is no concept of current ring.  When you assign a ring to a variable,
     the variables in the ring are made global variables.  To get the variables in a previous ring
     to be available, use ", TO (use,Ring), ".",
     EXAMPLE {
	  "x",
	  "use D",
	  "x"
	  },
     "Now x is an element of the ring D.",
     EXAMPLE {
	  "describe D"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.1.10",
     Headline => "methods for creating ring maps",
     "In Macaulay2, ring maps from a polynomial ring are defined and used as
     follows.",
     EXAMPLE {
	  "A = QQ[a,b,c];",
	  "f = a+b+a*b+c^3;",
	  "B = QQ[x,y,z];",
	  "F = map(B,A,{x+y, x-y, z})",
	  },
     "Notice that ring maps are defined by first giving the target ring,
     then the source ring, and finally the data.",
     PARA{},
     "Parentheses for functions with one parameter are optional.",
     EXAMPLE {
	  "g = F f",
	  "A1 = QQ[x,y,c,b,a,z];",
	  "substitute(f,A1)",
	  },
     "To map the first variable of A to the first variable of A1, the second variable of A to the second
     variable of A1, and so on, create the list of the first generators of A1",
     EXAMPLE {
          "v = take(gens A1, numgens A)",
	  "G = map(A1,A,v)",
	  "G f",
	  },
     SeeAlso => {"substitution and maps between rings", map, substitute, RingMap, generators, numgens, take}
     }

document {
     Key => "Singular Book 1.2.3",
     Headline => "leading data",
     EXAMPLE {
	  "A = QQ[x,y,z,MonomialOrder=>Lex];",
	  "f = y^4*z^3+2*x^2*y^2*z^2+3*x^5+4*z^4+5*y^2",
	  "leadMonomial f",
	  "exponents leadMonomial f",
	  "leadTerm f",
	  "leadCoefficient f",
	  "someTerms(f,1,size f - 1)",
	  "someTerms(f,1,-1)",
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.2.13",
     Headline => "monomial orderings",
     "Monomial orderings are specified when defining a polynomial ring.",
     SUBSECTION "global orderings",
     "The default order is the graded (degree) reverse lexicographic order.",
     EXAMPLE {
	  "A2 = QQ[x,y,z];",
	  "A2 = QQ[x,y,z,MonomialOrder=>GRevLex];",
	  "f = x^3*y*z+y^5+z^4+x^3+x*y^2"
	  },
     "Lexicographic order.",
     EXAMPLE {
	  "A1 = QQ[x,y,z,MonomialOrder=>Lex];",
	  "substitute(f,A1)"
	  },
     "Graded (degree) lexicographic order.",
     EXAMPLE {
	  "A3 = QQ[x,y,z,MonomialOrder=>{Weights=>{1,1,1},Lex}];",
	  "substitute(f,A3)"
	  },
     "Graded (degree) lexicographic order, with nonstandard weights.",
     EXAMPLE {
	  "A4 = QQ[x,y,z,MonomialOrder=>{Weights=>{5,3,2},Lex}];",
	  "substitute(f,A4)"
	  },
     "A product order, with each block being GRevLex.",
     EXAMPLE {
	  "A = QQ[x,y,z,MonomialOrder=>{1,2}];",
	  "substitute(f,A)"
	  },
     SUBSECTION "local orderings",
     "Negative lexicographic order.",
     EXAMPLE {
	  "A = QQ[x,y,z,MonomialOrder=>{Weights=>{-1,0,0},Weights=>{0,-1,0},Weights=>{0,0,-1}},Global=>false];",
	  "substitute(f,A)"
	  },
     "Negative graded reverse lexicographic order.",
     EXAMPLE {
	  "A = QQ[x,y,z,MonomialOrder=>{Weights=>{-1,-1,-1},GRevLex},Global=>false];",
	  "substitute(f,A)"
	  },

     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.3.3",
     Headline => "properties of ring maps",
     EXAMPLE {
	  "S = QQ[a,b,c];",
	  "R = QQ[x,y,z];",
	  "phi = map(R,S,{x,y,x^2-y^3})",
	  "isInjective phi",
	  -- "-- isSurjective phi",
	  -- "-- isIsomorphism phi",
	  "ker phi"
	  },
     "Packaged code for computing preimage is missing, but it's easy to do, as follows.",
     EXAMPLE {
     	  "psi = map(R,S,{x,x+y,z-x^2+y^3})",
     	  "isInjective psi",
     	  "ker psi"
     	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.3.13",
     Headline => "computation in quotient rings",
     "In Macaulay2, we define a quotient ring using the usual mathematical
     notation.",
     EXAMPLE {
	  "R = ZZ/32003[x,y,z];",
	  "Q = R/(x^2+y^2-z^5, z-x-y^2)",
	  "f = z^2+y^2",
	  "g = z^2+2*x-2*z-3*z^5+3*x^2+6*y^2",
	  "f == g",
	  },
     "Testing for zerodivisors in Macaulay2:",
     EXAMPLE {
	  "ann f",
	  },
     TEX "This is the zero ideal, meaning that $f$ is not a zero divisor in the
     ring $Q$.",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.3.15",
     Headline => "computing with radicals",
     "Compute the radical of an ideal with ", TO "MinimalPrimes::radical", ".",
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "radical ideal(z^4+2*z^2+1)",
	  },
     "A somewhat more complicated example:",
     EXAMPLE {
	  ///I = ideal"xyz,x2,y4+y5"///,
	  "radical I",
	  },
     TEX "The index of nilpotency.  We compute the minimal integer $k$ such that
     $(y^2+y)^k \\in I$.",
     EXAMPLE {
	  "k = 0;",
	  "while (y^2+y)^k % I != 0 do k = k+1;",
	  "k"
	  },
     "The index of nilpotency is 4.",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.4.9",
     Headline => "global versus local rings",
     "Consider the union of a line and a plane in affine 3-space.",
     EXAMPLE {
	  "S = QQ[x,y,z];",
	  "I = ideal(y*(x-1), z*(x-1));",
	  },
     "The dimension is 2, the maximum of the dimensions of the two components.  In order
     to find the dimension, Macaulay2 requires the Groebner basis of I.  It computes this 
     behind the scenes, and caches the value with I.",
     EXAMPLE {
	  "dim I",
	  "gens gb I",
	  },
     TEX "Notice that $y$ is not in $I$.",
     EXAMPLE {
     	  "y % I",
	  },
     "Now let's use a local order.",
     EXAMPLE {
	  "R = QQ[x,y,z,MonomialOrder=>{Weights=>{-1,-1,-1},RevLex},Global=>false];",
	  "J = substitute(I,R)",
	  "gens gb J",
	  },
     "The dimension in this case is 1.",
     EXAMPLE {
	  "dim J",
	  },
     PARA{},
     TEX "The following is WRONG.  In this local ring, $y$ is in the ideal $J$.",
     EXAMPLE {
     	  "y % J",
	  },
     PARA{},     
     TEX "Translate the origin to $(1,0,0)$.  The plane $x-1 = 0$ goes through this new origin.",
     EXAMPLE {
	  "J = substitute(J, {x=>x+1})",
	  "dim J",
	  },
     PARA{},
     "Compute the global dimension after translation.",
     EXAMPLE {
     	  "use ring I",
	  "I1 = substitute(I, {x=>x+1})",
	  "dim I1",
	  },
     "See also ", TO dim, ".",
     SeeAlso => {dim}
     }
document {
     Key => "Singular Book 1.5.10",
     Headline => "realization of rings",
     "We define the rings of example 1.5.3, in the Singular book.",
     EXAMPLE {
	  "(n,m) = (2,3);",
	  "A1 = QQ[x_1..x_n,y_1..y_m,MonomialOrder=>{n, RevLex=>m},Global=>false];",
	  "f = x_1*x_2^2 + 1 + y_1^10 + x_1*y_2^5 + y_3",
	  "1_A1 > y_1^10",
	  },
     PARA{},
     "The second monomial order has the first block local, and the second block polynomial.",
     EXAMPLE {
	  "A2 = QQ[x_1..x_n,y_1..y_m,MonomialOrder=>{RevLex=>n, m},Global=>false];",
	  "substitute(f,A2)",
	  "x_1*y_2^5 < 1_A2",
	  },
     PARA{},
     "The third example has three blocks of variables.",
     EXAMPLE {
	  "A3 = QQ[x_1..x_n,y_1..y_m,MonomialOrder=>{n, RevLex=>2, m-2},Global=>false];",
	  "substitute(f,A3)",
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.6.13",
     Headline => "normal form",
     "Normal forms in Macaulay2 are done using the remainder operator ", TO symbol%, ".",
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  "f = x^2*y*z+x*y^2*z+y^2*z+z^3+x*y;",
	  "f1 = x*y+y^2-1",
	  "f2 = x*y",
	  "G = ideal(f1,f2)"
	  },
     "Macaulay2 computes a Groebner basis of G, and uses that to find the
     normal form of f. 
     In Macaulay2, all remainders are reduced normal forms (at least for non-local orders).",
     EXAMPLE {
	  "f % G"
	  },
     PARA{},
     "In order to reduce using a non Groebner basis, use ", TO forceGB,
     EXAMPLE {
	  "f % (forceGB gens G)"
	  },
     "This is a different answer from the SINGULAR book, since the choice of
     divisor affects the answer.",
     EXAMPLE {
	  "f % (forceGB matrix{{f2,f1}})"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.7.10",
     Headline => "standard bases",
     "We show the Groebner and standard bases of an ideal under several different
     orders and localizations.  First, the default order is graded (degree) reverse
     lexicographic.",
     EXAMPLE {
     	  "A = QQ[x,y];",
     	  ///I = ideal "x10+x9y2,y8-x2y7";///,
     	  "transpose gens gb I"
	  },
     PARA{},
     "Lexicographic order:",
     EXAMPLE {
	  "A1 = QQ[x,y,MonomialOrder=>Lex];",
	  "I = substitute(I,A1)",
	  "transpose gens gb I"
	  },
     PARA{},
     "Now we change to a local order",
     EXAMPLE {
	  "B = QQ[x,y,MonomialOrder=>{Weights=>{-1,-1},2},Global=>false];",
	  "I = substitute(I,B)",
	  "transpose gens gb I"
	  },
     PARA{},
     "Another local order: negative lexicographic.",
     EXAMPLE {
	  "B = QQ[x,y,MonomialOrder=>{Weights=>{-1,0},Weights=>{0,-1}},Global=>false];",
	  "I = substitute(I,B)",
	  "transpose gens gb I"
	  },
     PARA{},
     "One method to compute a standard basis is via homogenization.  The example
     below does this, obtaining a standard basis which is not minimal.",
     EXAMPLE {
	  "M = matrix{{1,1,1},{0,-1,-1},{0,0,-1}}",
	  "mo = apply(entries M, e -> Weights => e)",
	  "C = QQ[t,x,y,MonomialOrder=>mo];",
	  "I = homogenize(substitute(I,C),t)",
          "transpose gens gb I",
	  "substitute(transpose gens gb I, {t=>1})"
	  },
     "The first two elements form a standard basis.",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.7.12",
     Headline => "highest corner",
     "This is not implemented in macaulay2 yet.",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.8.1",
     Headline => "ideal membership",
     EXAMPLE {
	  "A = QQ[x,y];",
	  "I = ideal(x^10+x^9*y^2, y^8-x^2*y^7);",
	  "f = x^2*y^7+y^14;",
	  "f % I"
	  },
     "So this f is not in the ideal I.",
     EXAMPLE {
	  "f = x*y^13+y^12;",
	  "f % I"
	  },
     "This f is in the ideal I.",
     PARA{},
     "Check inclusion and equality of ideals.",
     EXAMPLE {
	  "K = ideal(f,x^2*y^7+y^14);",
	  "(gens K) % I"
	  },
     "In Macaulay2, inclusion of ideals can be tested using ", TO (isSubset,Ideal,Ideal),
     " and equality can be checked using ", TO (symbol==,Ideal,Ideal), ".  In both cases
     the necessary Groebner bases are computed, if they have not already been computed.",
     EXAMPLE {
	  "isSubset(K,I)",
	  "K == I"
	  },
     EXAMPLE {
	  "K = ideal(f,y^14+x*y^12);",
	  "(gens K) % I",
	  "isSubset(K,I)",
	  "K == I"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.8.2",
     Headline => "linear combination of ideal members",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.8.4",
     Headline => "elimination of variables",
     "There are several methods to eliminate variables in Macaulay2.",
     EXAMPLE {
	  "A = QQ[t,x,y,z];",
	  ///I = ideal"t2+x2+y2+z2,t2+2x2-xy-z2,t+y3-z3";///,
	  "eliminate(I,t)"
	  },
     PARA{},
     "Alternatively, one may do it by hand: the elements of the Groebner basis
     under an elimination order not involving ", TT "t", " generate the elimination ideal.",
     EXAMPLE {
	  "A1 = QQ[t,x,y,z,MonomialOrder=>{1,3}];",
	  "I = substitute(I,A1);",
	  "transpose gens gb I"
	  },
     PARA{},
     "Here is another elimination ideal.  Weights not given are assumed to be zero.",
     EXAMPLE {
	  "A2 = QQ[t,x,y,z,MonomialOrder=>Weights=>{1}];",
	  "I = substitute(I,A2);",
	  "transpose gens gb I"
	  },
     PARA{},
     "The same order as the previous one:",
     EXAMPLE {
	  "A3 = QQ[t,x,y,z,MonomialOrder=>Eliminate 1];",
	  "I = substitute(I,A3);",
	  "transpose gens gb I"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.8.6",
     Headline => "Zariski closure of the image",
     "We compute an implicit equation for the surface defined parametrically by the map ", 
     TEX "$f : A^2 \\rightarrow{} A^3, (u,v) \\mapsto{} (uv,uv^2,u^2)$", ".",
     EXAMPLE {
	  ///A = QQ[u,v,x,y,z];///,
	  ///I = ideal "x-uv,y-uv2,z-u2"///,
          ///eliminate(I,{u,v})///
	  },
     TEX "This ideal defines the closure of the map $f$, the Whitney umbrella.",
     PARA {
	  "Alternatively, we could take the coimage of the ring homomorphism ", TT "g", " corresponding to ", TT "f", "."
	  },
     EXAMPLE {
	  "g = map(QQ[u,v],QQ[x,y,z],{x => u*v, y => u*v^2, z => u^2})",
	  "coimage g",
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.8.7",
     Headline => "solving equations",
     EXAMPLE {
	  "A = QQ[x,y,z,MonomialOrder=>Lex];",
	  "I = ideal(x^2+y+z-1, x+y^2+z-1, x+y+z^2-1);",
	  "transpose gens gb I"
	  },
     "Now we need to implement the solver!",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.8.9",
     Headline => "radical membership",
     TEX "Recall that an element $f$ is in an ideal $I$ if
     $1 \\in (I, tf-1) \\subset R[t]$.",
     EXAMPLE {
	  "A = QQ[x,y,z];",
	  ///I = ideal"x5,xy3,y7,z3+xyz";///,
	  "f = x+y+z;"
	  },
     EXAMPLE {
	  "B = A[t];",
	  "J = substitute(I,B) + ideal(f*t-1)",
	  "1 % J "
	  },
     "The polynomial f is in the radical. Let's compute the radical to make sure.",
     EXAMPLE {
	  "radical I"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.8.11",
     Headline => "intersection of ideals",
     "Intersecting ideals using the Macaulay2 ", TO intersect, " function.",
     EXAMPLE {
	  "A = QQ[x,y,z];",
	  "I1 = ideal(x,y);",
	  "I2 = ideal(y^2,z);",
	  "intersect(I1,I2)"
	  },
     "Now we use the method described in the Singular book in section 1.8.7.",
     EXAMPLE {
	  "B = QQ[t,x,y,z];",
	  "I1 = substitute(I1,B);",
	  "I2 = substitute(I2,B);",
	  "J = t*I1 + (1-t)*I2",
          "eliminate(J,t)"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.8.13",
     Headline => "quotient of ideals",
     EXAMPLE {
	  "A = QQ[x,y,z];",
	  "I1 = ideal(x,y);",
	  "I2 = ideal(y^2,z);",
	  "I1 : I2"
	  },
     "The quotient function is the same as the colon operator, except that
     optional arguments may be given.",
     EXAMPLE {
	  "quotient(I1,I2)"
	  },
     "Now we use the method described in Lemma 1.8.12 in the Singular book.",
     EXAMPLE {
	  "J1 = intersect(I1,ideal(I2_0))",
	  "J2 = intersect(I1,ideal(I2_1))",
	  },
     "Now divide each generator of J1 by x, and each generator of J2 by y.  Notice that
     division uses two slashes.  Using only one slash gives the quotient in the fraction ring.",
     EXAMPLE {
	  "K1 = ideal(J1_0//I2_0)",
	  "K2 = ideal(J2_0//I2_1, J2_1//I2_1)",
	  "intersect(K1,K2)"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.8.15",
     Headline => "saturation",
     EXAMPLE {
	  "A = QQ[x,y,z];",
	  "I1 = ideal(x^5*z^3, x*y*z, y*z^4);",
	  "saturate(I1,z)"
	  },
     "Now we compute the saturation using a loop.",
     EXAMPLE {
	  "J = I1:z",
	  "k = 0;",
	  "while not isSubset(J,I1) do (
   k = k+1;
   I1 = J;
   J = I1 : z;
   );",
	  "J",
	  "k"
	  },
     "We needed to use quotient four times.",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.8.18",
     Headline => "kernel of a ring map",
     "First, we use Macaulay2's ", TO (kernel,RingMap), " function.",
     EXAMPLE {
	  "A = QQ[x,y,z];",
	  "B = QQ[a,b];",
	  "phi = map(B,A,{a^2,a*b,b^2})",
	  "kernel phi"
	  },
     "Now use the elimination of variables method.",
     EXAMPLE {
	  "C = QQ[x,y,z,a,b]",
	  "H = ideal(x-a^2, y-a*b, z-b^2);",
          "eliminate(H, {a,b})"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.8.19",
     Headline => "algebraic dependence",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 1.8.20",
     Headline => "subalgebra membership",
     SeeAlso => {}
     }

document {
     Key => "Singular Book 2.1.6",
     Headline => "matrix operations",
     "In Macaulay2, matrices are defined over a ring.  There are many ways to make 
     a matrix, but the easiest is to use the ", TO matrix, " routine.",
     EXAMPLE {
	  "A = QQ[x,y,z];",
"M = matrix{{1, x+y, z^2},
           {x, 0,   x*y*z}}",
	  "N = matrix(A, {{1,2,3},{4,5,6},{7,8,9}})",
	  },
     "The usual matrix arithmetic operations work.",
     EXAMPLE {
	  "M+M",
	  "x*N",
	  "M*N",
	  "N^3",
	  "((x+y+z)*N)^3"
	  },
     "Indices in Macaulay2 are always 0 based, so the upper left
     entry is (0,0).  Indexing is performed using ", TO symbol_, ".",
     EXAMPLE {
	  "M_(1,2)"
	  },
     "Matrices cannot be modified.  Make a MutableMatrix if you want to modify
     a matrix.",
     EXAMPLE {
	  "M1 = mutableMatrix M",
	  "M1_(1,2) = 37_A",
	  "M1",
	  "matrix M1"
	  },
     "Matrices can be concatenated, either horizontally or vertically.  The number of
     rows must match for horizontal concatenation, and the number of columns must 
     match for vertical concatenation.",
     EXAMPLE {
	  "M | M",
	  "M || N"
	  },
     "Use ", TO (ideal,Matrix), " to obtain the ideal generated by the entries of a matrix.",
     EXAMPLE {
	  "ideal M"
	  },
     TEX "The $n$ by $n$ identity matrix is the identity map of the freemodule $A^n$.",
     EXAMPLE {
	  "F = A^5",
	  "id_(A^5)"
	  },
     "In Macaulay2, integer matrices are just matrices defined over the ring of integers ZZ.",
     EXAMPLE {
	  "matrix{{1,2,3},{4,5,6}}"
	  },
     SeeAlso => {"matrices"}
     }
document {
     Key => "Singular Book 2.1.7",
     Headline => "maps induced by Hom",
     EXAMPLE {
	  "A = QQ[x,y,z]",
	  "M = matrix(A, {{1,2,3},{4,5,6},{7,8,9}})",
	  "Hom(M,A^2)",
	  "Hom(A^2,M)"
	  },
     "Notice that the basis that Macaulay2 uses for Hom(A^3,A^2) is different than the basis
     used by Singular.",
     PARA{},
     "The function contraHom of the Singular book in example 2.1.7 could be coded in 
     the following way.",
     EXAMPLE {
"contraHom = (M, s) -> (
    (n,m) := (numgens target M, numgens source M);
    R := mutableMatrix(ring M, s*n, s*m);
    for b from 0 to m-1 do
      for a from 0 to s-1 do
        for c from 0 to n-1 do
          R_(a*n+c,a*m+b) = M_(b,c);
    matrix R
    )",
     },
     "Let's try an example.",
     EXAMPLE {
	  "contraHom(M,2)"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.1.10",
     Headline => "Submodules of A^n",
     "A common method of creating a submodule of A^n in Macaulay2 is to take the image
     of a matrix.  This will be a submodule generated by the columns of the matrix.",
     EXAMPLE {
	  "A = QQ[x,y,z];",
	  "f = matrix{{x*y-1,y^4},{z^2+3,x^3},{x*y*z,z^2}}",
	  "M = image f",
	  "numgens M",
	  "ambient M"
	  },
     "A submodule can easily be moved to quotient rings.",
     EXAMPLE {
	  "Q = A/(x^2+y^2+z^2);",
	  "substitute(M,Q)"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.1.13",
     Headline => "kernel, image and cokernel of a module homomorphism",
     "In Macaulay2, a Matrix is the same thing as a module homomorphism.
     The computation of the kernel of a module homomorphism is based on 
     a Groebner basis computation.",
     EXAMPLE {
	  "A = QQ[x,y,z];",
	  "M = matrix{{x,x*y,z},{x^2,x*y*z,y*z}}",
	  "K = kernel M",
	  },
     "The image and cokernel of a matrix require no computation.",
     EXAMPLE {
	  "I = image M",
	  "N = cokernel M",
	  "P = coimage M"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.1.20",
     Headline => "sum, intersection, module quotient",
     EXAMPLE {
	  "A = QQ[x,y,z];",
	  "M = image matrix{{x*y,x},{x*z,x}}",
	  "N = image matrix{{y^2,x},{z^2,x}}",
	  "M + N"
	  },
     "Notice that, in Macaulay2, each module comes equipped with a 
     list of generators, and operations such as sum do not try to
     simplify the list of generators.",
     PARA{},
     "Intersection, quotients, annihilators are found using standard notation:",
     EXAMPLE {
	  "intersect(M,N)",
	  "M : N",
	  "N : M",
	  "Q = A/x^5;",
	  "M = substitute(M,Q)",
	  "ann M",
	  "M : x"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.1.24",
     Headline => "submodules, presentation of a module",
     EXAMPLE {
	  "A = QQ[x,y,z]",
	  "N = image matrix{{x*y,0},{0,x*z},{y*z,z^2}}"
	  },
     "The submodule is generated by the two columns of this matrix.",
     EXAMPLE {
	  "N + x*N",
	  },
     "It is easy to go between matrices and submodules.  Use ", TO (gens,Module),
     " and ", TO (image,Matrix), "(", TT "gens", " and ", TT "generators", " are synonyms).  
     There is no automatic conversion between modules and
     matrices in Macaulay2.",
     EXAMPLE {
	  "f = matrix{{x*y,x*z},{y*z,z^2}}",
	  "M = image f",
	  "g = gens M",
	  "f == g"
	  },
     "In Macaulay2, matrices are not automatically either presentation matrices or generating
     matrices for a module.  You use whichever you have in mind.",
     EXAMPLE {
	  "N = cokernel f",
	  "presentation N",
	  "presentation M"
	  },
     "Notice that the presentation of N requires no computation, whereas the presentation of 
     M requires a syzygy computation.",
     PARA{},
     TO (kernel,Matrix), " gives a submodule, while ", TO (syz,Matrix), " returns the matrix.",
     EXAMPLE {
	  "syz f",
	  "kernel f"
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.1.26",
     Headline => "computation of Hom",
     EXAMPLE {
	  "A = QQ[x,y,z];",
	  "M = cokernel matrix(A, {{1,2,3},{4,5,6},{7,8,9}})",
	  "N = cokernel matrix{{x,y},{z,0}}",
	  "H = Hom(M,N)"
	  },
     "H is a subquotient module.  In Macaulay2, the most general form of a
     module is as a subquotient: a submodule of a cokernel module.  For more about
     subquotient modules, see ", TO "modules", ".  ",
     -- Mike wanted this: "For more about Hom and their corresponding homomorphisms, see ", TO "Hom and homomorphisms", ".",
     EXAMPLE {
	  "f = homomorphism H_{0}",
          "target f === N",
	  "source f === M",
	  "matrix f"
	  },
     "Macaulay2 has a ", TT "modulo", " command (it was initially introduced in the original Macaulay, 
     in the late 1980's), but it is not needed very often.  It is used internally in Macaulay2 to implement
     kernels of module homomorphisms.",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.1.34",
     Headline => "minimal presentations, prune",
     EXAMPLE {
     	  "R = QQ[x,y,z,MonomialOrder=>{Weights=>{-1,-1,-1},RevLex},Global=>false]",
     	  "M = cokernel matrix{{0,y},{x*y-1,x*z},{x*y+1,x*z}}",
     	  "null -- prune M -- doesn't work yet",
     	  ///stderr << "--this example doesn't work yet" << endl;///
	  },
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.2.15",
     Headline => "graded rings and modules",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.3.10",
     Headline => "normal form",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.3.12",
     Headline => "standard bases",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.4.12",
     Headline => "resolution and Betti numbers",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.4.15",
     Headline => "homogeneous resolution and graded Betti numbers",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.5.5",
     Headline => "syzygies",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.5.18",
     Headline => "Schreyer resolution",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.6.3",
     Headline => "diagonal form",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.6.11",
     Headline => "cyclic decomposition",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.6.15",
     Headline => "Jordan normal form",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.7.5",
     Headline => "tensor product of maps",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.7.9",
     Headline => "tensor product of modules",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.7.14",
     Headline => "tensor product of rings",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.8.1",
     Headline => "module membership",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.8.3",
     Headline => "elimination of module components",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.8.5",
     Headline => "intersection of modules",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.8.6",
     Headline => "quotient of modules",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.8.7",
     Headline => "radical, zerodivisors of modules",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.8.8",
     Headline => "annihilator and Fitting ideal",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.8.9",
     Headline => "kernel of a module homomorphism",
     SeeAlso => {}
     }
document {
     Key => "Singular Book 2.8.10",
     Headline => "solving linear equations",
     SeeAlso => {}
     }
