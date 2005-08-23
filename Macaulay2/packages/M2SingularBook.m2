newPackage(
	"M2SingularBook",
    	Version => "1.0", 
    	Date => "August 23, 2005",
    	Authors => {
	     {Name => "Mike Stillman", Email => "mike@math.cornell.edu"}
	     },
    	HomePage => "http://www.math.cornell.edu/~mike/",
    	Headline => "Macaulay2 examples for the Singular book",
    	DebuggingMode => true 
    	)

export (showPDF, poly)

poly = method()
poly String := (f) -> (ideal f)_0

showPDF = x -> (
     f := temporaryFileName();
     f | ".tex" 
     << ///\documentclass{article}
     \usepackage{amsmath}
     \usepackage{amssymb}
     \begin{document}
     ///  
     << tex x <<
     ///
     \end{document}
     ///  
     << close;
     if 0 === run("cd /tmp; pdflatex " | f)
     then run("(open "|f|".pdf; rm -f "|f|".tex "|f|".dvi "|f|".log "|f|".aux)&")
     else error ("latex failed on input file " | f | ".tex")
     )

beginDocumentation()
document { 
	Key => M2SingularBook,
    	Headline => "Macaulay2 examples for the Singular book",
    	EM "M2SingularBook", " consists of Macaulay2 translations of the examples in the book ",
	      EM "A Singular introduction to commutative algebra",
	PARA,
	"Each example is numbered as it is in Greuel and Pfister's book.  Links to 
	Macaulay2 routines and concepts are also provided.",
	PARA,
	TO "Macaulay2 implementation bugs and problems",
	Subnodes => {
	     "Chapter 1",
	     TO "1.1.8",
	     TO "1.1.9",
	     TO "1.1.10",
	     TO "1.2.3",
	     TO "1.2.13",
	     TO "1.3.3",
	     TO "1.3.13",
	     TO "1.3.15",
	     TO "1.4.9",
	     TO "1.5.10",
	     TO "1.6.13",
	     TO "1.7.10",
	     TO "1.7.12",
	     TO "1.8.1",
	     TO "1.8.2",
	     TO "1.8.4",
	     TO "1.8.6",
	     TO "1.8.7",
	     TO "1.8.9",
	     TO "1.8.11",
	     TO "1.8.13",
	     TO "1.8.15",
	     TO "1.8.18",
	     TO "1.8.19",
	     TO "1.8.20"
	     }
	}
document {
     Key => "Macaulay2 implementation bugs and problems",
     UL {
	  "poly should be allowed for short input of polynomials?",
	  "Some monomial orders should be easier to specify",
	  "Local remainder is wrong",
	  "Local GB can be improved: x13+x14y can be reduced??",
	  "Highest corner use in local computations can speed things up...",
	  "1.8.2: how are we doing local division?",
	  "1.8.2: write this node",
	  "1.8.7: solving equations is not quite implemented.  
	    But it should not be that hard...",
	  "1.8.15: implement sat, saturation which returns (Isat,ZZ).",
	  "1.8.18, 1.8.19: write these",
	  }
     },
document {
     Key => "1.1.8",
     Headline => "computation in fields",
     SeeAlso => {}
     }
document {
     Key => "1.1.9",
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
     Key => "1.1.10",
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
     PARA,
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
     SeeAlso => {"ring maps", map, substitute, RingMap, generators, numgens, take}
     }

document {
     Key => "1.2.3",
     Headline => "leading data",
     EXAMPLE {
	  "A = QQ[x,y,z,MonomialOrder=>Lex];",
	  ///f = poly "y4z3+2x2y2z2+3x5+4z4+5y2"///,
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
     Key => "1.2.13",
     Headline => "monomial orderings",
     SeeAlso => {}
     }
document {
     Key => "1.3.3",
     Headline => "properties of ring maps",
     EXAMPLE {
	  "S = QQ[a,b,c];",
	  "R = QQ[x,y,z];",
	  "phi = map(R,S,{x,y,x^2-y^3})",
	  "isInjective phi",
	  "-- isSurjective phi",
	  "-- isIsomorphism phi",
	  "ker phi",
	  "-- preimage is missing, but easy to do",
     	  "psi = map(R,S,{x,x+y,z-x^2+y^3})",
     	  "isInjective psi",
     	  "ker psi",
     	  },
     SeeAlso => {}
     }
document {
     Key => "1.3.13",
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
     Key => "1.3.15",
     Headline => "computing with radicals",
     "Compute the radical of an ideal with ", TO radical, ".",
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
     Key => "1.4.9",
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
	  "R = QQ[x,y,z,MonomialOrder=>{Weights=>{-1,-1,-1},RevLex}];",
	  "J = substitute(I,R)",
	  "gens gb J",
	  },
     "The dimension in this case is 1.",
     EXAMPLE {
	  "dim J",
	  },
     PARA,
     TEX "The following is WRONG.  In this local ring, $y$ is in the ideal $J$.",
     EXAMPLE {
     	  "y % J",
	  },
     PARA,     
     TEX "Translate the origin to $(1,0,0)$.  The plane $x-1 = 0$ goes through this new origin.",
     EXAMPLE {
	  "J = substitute(J, {x=>x+1})",
	  "dim J",
	  },
     PARA,
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
     Key => "1.5.10",
     Headline => "realization of rings",
     "We define the rings of example 1.5.3, in the Singular book.",
     EXAMPLE {
	  "(n,m) = (2,3);",
	  "A1 = QQ[x_1..x_n,y_1..y_m,MonomialOrder=>{n, RevLex=>m}];",
	  "f = x_1*x_2^2 + 1 + y_1^10 + x_1*y_2^5 + y_3",
	  "1_A1 > y_1^10",
	  },
     PARA,
     "The second monomial order has the first block local, and the second block polynomial.",
     EXAMPLE {
	  "A2 = QQ[x_1..x_n,y_1..y_m,MonomialOrder=>{RevLex=>n, m}];",
	  "substitute(f,A2)",
	  "x_1*y_2^5 < 1_A2",
	  },
     PARA,
     "The third example has three blocks of variables.",
     EXAMPLE {
	  "A3 = QQ[x_1..x_n,y_1..y_m,MonomialOrder=>{n, RevLex=>2, m-2}];",
	  "substitute(f,A3)",
	  },
     SeeAlso => {}
     }
document {
     Key => "1.6.13",
     Headline => "normal form",
     "Normal forms in Macaulay2 are done using the remainder operator ", TO symbol%, ".",
     EXAMPLE {
	  "R = QQ[x,y,z];",
	  ///f = poly "x2yz+xy2z+y2z+z3+xy";///,
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
     PARA,
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
     Key => "1.7.10",
     Headline => "standard bases",
     "We show the Groebner and standard bases of an ideal under several different
     orders and localizations.  First, the default order is graded (degree) reverse
     lexicographic.",
     EXAMPLE {
     	  "A = QQ[x,y];",
     	  ///I = ideal "x10+x9y2,y8-x2y7";///,
     	  "transpose gens gb I"
	  },
     PARA,
     "Lexicographic order:",
     EXAMPLE {
	  "A1 = QQ[x,y,MonomialOrder=>Lex];",
	  "I = substitute(I,A1)",
	  "transpose gens gb I"
	  },
     PARA,
     "Now we change to a local order",
     EXAMPLE {
	  "B = QQ[x,y,MonomialOrder=>{Weights=>{-1,-1},2}];",
	  "I = substitute(I,B)",
	  "transpose gens gb I"
	  },
     PARA,
     "Another local order: negative lexicographic.",
     EXAMPLE {
	  "B = QQ[x,y,MonomialOrder=>{Weights=>{-1,0},Weights=>{0,-1}}];",
	  "I = substitute(I,B)",
	  "transpose gens gb I"
	  },
     PARA,
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
     Key => "1.7.12",
     Headline => "highest corner",
     "This is not implemented in macaulay2 yet.",
     SeeAlso => {}
     }
document {
     Key => "1.8.1",
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
     PARA,
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
     Key => "1.8.2",
     Headline => "linear combination of ideal members",
     SeeAlso => {}
     }
document {
     Key => "1.8.4",
     Headline => "elimination of variables",
     "There are several methods to eliminate variables in Macaulay2.",
     EXAMPLE {
	  ///loadPackage "Elimination";///,
	  "A = QQ[t,x,y,z];",
	  ///I = ideal"t2+x2+y2+z2,t2+2x2-xy-z2,t+y3-z3";///,
	  "eliminate(I,t)"
	  },
     PARA,
     "Alternatively, one may do it by hand: the elements of the Groebner basis
     under an elimination order not involving ", TT "t", " generate the elimination ideal.",
     EXAMPLE {
	  "A1 = QQ[t,x,y,z,MonomialOrder=>{1,3}];",
	  "I = substitute(I,A1);",
	  "transpose gens gb I"
	  },
     PARA,
     "Here is another elimination ideal.  Weights not given are assumed to be zero.",
     EXAMPLE {
	  "A2 = QQ[t,x,y,z,MonomialOrder=>Weights=>{1}];",
	  "I = substitute(I,A2);",
	  "transpose gens gb I"
	  },
     PARA,
     "The same order as the previous one:",
     EXAMPLE {
	  "A3 = QQ[t,x,y,z,MonomialOrder=>Eliminate 1];",
	  "I = substitute(I,A3);",
	  "transpose gens gb I"
	  },
     SeeAlso => {}
     }
document {
     Key => "1.8.6",
     Headline => "Zariski closure of the image",
     "We compute an implicit equation for the surface defined parametrically by the map ", 
     TEX "$f : A^2 \\rightarrow A^3, (u,v) \\mapsto (uv,uv^2,u^2)$", ".",
     EXAMPLE {
	  ///loadPackage "Elimination";///,
	  ///A = QQ[u,v,x,y,z];///,
	  ///I = ideal "x-uv,y-uv2,z-u2";///,
          ///eliminate(I,{u,v})///
	  },
     TEX "This ideal defines the closure of the map $f$, the Whitney umbrella.",
     SeeAlso => {}
     }
document {
     Key => "1.8.7",
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
     Key => "1.8.9",
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
     Key => "1.8.11",
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
          ///loadPackage "Elimination";///,
          "eliminate(J,t)"
	  },
     SeeAlso => {}
     }
document {
     Key => "1.8.13",
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
     Key => "1.8.15",
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
     Key => "1.8.18",
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
     Key => "1.8.19",
     Headline => "algebraic dependence",
     SeeAlso => {}
     }
document {
     Key => "1.8.20",
     Headline => "subalgebra membership",
     SeeAlso => {}
     }
       
end

document {
     Key => "",
     Headline => "",
     SeeAlso => {}
     }
document {
     Key => "",
     Headline => "",
     SeeAlso => {}
     }

installPackage "M2SingularBook"
installPackage("M2SingularBook", RemakeAllDocumentation=>true)
check M2SingularBook


