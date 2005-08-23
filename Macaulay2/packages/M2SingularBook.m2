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

export showPDF

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
     Key => "1.1.8",
     Headline => "computation in fields",
     SeeAlso => {}
     }
document {
     Key => "1.1.9",
     Headline => "computation in polynomial rings",
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
	  "poly = method();",
	  "poly String := (f) -> (ideal f)_0;",
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
     SeeAlso => {}
     }
document {
     Key => "1.6.13",
     Headline => "normal form",
     SeeAlso => {}
     }
document {
     Key => "1.7.10",
     Headline => "standard bases",
     SeeAlso => {}
     }
document {
     Key => "1.7.12",
     Headline => "highest corner",
     SeeAlso => {}
     }
document {
     Key => "1.8.1",
     Headline => "ideal membership",
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
     SeeAlso => {}
     }
document {
     Key => "1.8.6",
     Headline => "Zariski closure of the image",
     SeeAlso => {}
     }
document {
     Key => "1.8.7",
     Headline => "solving equations",
     SeeAlso => {}
     }
document {
     Key => "1.8.9",
     Headline => "radical membership",
     SeeAlso => {}
     }
document {
     Key => "1.8.11",
     Headline => "intersection of ideals",
     SeeAlso => {}
     }
document {
     Key => "1.8.13",
     Headline => "quotient of ideals",
     SeeAlso => {}
     }
document {
     Key => "1.8.15",
     Headline => "saturation",
     SeeAlso => {}
     }
document {
     Key => "1.8.18",
     Headline => "kernel of a ring map",
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


