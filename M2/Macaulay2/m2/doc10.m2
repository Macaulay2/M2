--		Copyright 1993-1999 by Daniel R. Grayson


TEST ///
     R = QQ[x,y,z]
     C = res coker vars R
     D = C ++ C
     E = ker D_[0]
     E = coker D_[0]
     E = image D_[0]
     E = coimage D_[0]
///

document { symbol Resolution,
     "A key used in a ", TO "ChainComplex", " to store the resolution it comes from."
     }

document { Resolution, HEADLINE "the class of all resolution computations",
    PARA,
    "These resolutions are internal engine objects not meant to be examined
    by the user.",
    }

document { symbol "res",
    "See ", TO "resolution", ", of which ", TT "res", " is a synonym."
    }

document { resolution, HEADLINE "make a projective resolution" }

document { resolution => DegreeLimit,
     HEADLINE "compute only up to this degree",
     TT "DegreeLimit => n", " -- keyword for an optional argument used with
     ", TO "resolution", " which specifies that the computation should halt
     after dealing with degree n.",
     PARA,
     "This option is relevant only for homogeneous modules.",
     PARA,
     "One might get some matrix entries of slightly higher degree than requested.",
     EXAMPLE {
	  "R = ZZ/101[x,y,z,w];",
      	  "M = cokernel matrix {{x*y-z^2,y^2-w^2}}",
      	  "res(M,DegreeLimit => 1)",
      	  "res(M,DegreeLimit => 2)"
	  },
     }

document { resolution => SyzygyLimit,
     HEADLINE "stop when this number of syzygies are obtained",
     TT "SyzygyLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ", which specifies that the computation should
     stop after a certain number of syzygies have computed.",
     PARA,
     EXAMPLE {
	  "R = ZZ/101[x,y,z,w];",
      	  "M = cokernel matrix {{x*y-z^2,y^2-w^2,w^4}}",
      	  "res(M,SyzygyLimit => 1)",
      	  "res(M,SyzygyLimit => 2)",
      	  "res(M,SyzygyLimit => infinity)"
	  }
     }

document { resolution => PairLimit,
     HEADLINE "stop when this number of pairs are handled",
     TT "PairLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ", which specifies that the computation should
     be stopped after a certain number of S-pairs have been reduced.",
     EXAMPLE {
	  "R = QQ[x,y,z,w]",
      	  "M = cokernel matrix {{x*y-z,y^2-w-1,w^4-3}}",
      	  "res(M, PairLimit => 1)",
      	  "res(M, PairLimit => 10)",
      	  "res(M, PairLimit => 20)"
	  }
     }

document { resolution => StopBeforeComputation,
     HEADLINE "whether to stop the computation immediately",
     TT "StopBeforeComputation", " -- keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA,
     "Tells whether to start the computation, with the default value
     being ", TT "true", ".  This can be useful when you want to obtain
     the partially computed resolution contained in an interrupted computation."
     }

document { LengthLimit,
     HEADLINE "stop when the resolution reaches this length",
     }

document { Algorithm,
     HEADLINE "which algorithm to use"
     }

document { resolution => LengthLimit,
     HEADLINE "stop when the resolution reaches this length",
     TT "LengthLimit", " -- keyword for an optional argument used with
     ", TO "resolution", " which indicates how long a resolution to make.",
     PARA,
     "For polynomial rings over a field or over the integers, the length
     is taken to be the dimension of the ring, so the complete resolution will
     be obtained.  For quotient rings of such rings, the same number is used,
     so the complete resolution may not be obtained.",
     PARA,
     "In the current version, asking for a second and longer resolution of the
     same module involves recomputing the resolution from scratch.  Eventually
     the previous work will be used and the recomputation will go quicker.",
     PARA,
     "The resolution returned may actually be one step longer than requested.
     The extra differential is not guaranteed to be minimal."
     }

document { HardDegreeLimit,
     TT "HardDegreeLimit", " -- keyword for an optional argument which specifies
     that information above a specified degree is to be discarded."
     }

document { resolution => HardDegreeLimit,
     HEADLINE "always compute only up to this degree",
     TT "HardDegreeLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA,
     "The default value is ", TT "{}", ".",
     PARA,
     "Information above the specified degree is discarded."
     }

document { resolution => Algorithm,
     HEADLINE "which algorithm to use",
     TT "Algorithm => n", " -- an option for ", TO "resolution", " which specifies
     which algorithm to use.  Algorithms are specified by number and the
     algorithms available are",
     MENU {
	  (TT "Algorithm => 0", " -- Compute syzygies on the Groebner bases of each syzygy
	       module.  The algorithm uses important speedups due to R. Lascala.
	       This algorithm appears to be on the average the fastest."),
	  (TT "Algorithm => 1", " -- An older version of algorithm 0, which doesn't allow as 
	       much experimentation, but can sometimes be marginally faster."),
	  (TT "Algorithm => 2", " -- Compute syzygies on the minimal generators of each 
	       matrix in the resolution.  Over quotient rings, preferred."),
	  (TT "Algorithm => 3", " -- Same as algorithm 2, but compute those Hilbert functions 
	       which allow removal of S-pairs (a la Robbiano, et al.).  
	       Sometimes this improvement can be very dramatic.")
	  },
     "All algorithms use induced monomial orders (Schreyer orders), since 
     this makes an enormous improvement to the efficiency of the algorithm."
     }

document { SortStrategy,
     TT "SortStrategy", " -- an keyword for an optional argument which 
     specifies the strategy to be used for sorting S-pairs."
     }

document { resolution => SortStrategy,
     HEADLINE "specify strategy for sorting S-pairs",
     TT "SortStrategy => n", " -- an option for ", TO "resolution", " which 
     specifies the strategy to be used for sorting S-pairs.",
     PARA,
     "Not implemented yet."
     }

TEST "
R = ZZ/101[x,y]
M = cokernel matrix {{x^2+y^4, y^2 + x*y^3 + 11, 1 + x*y^2}}
C = res M
assert (HH_-1 C == 0)
assert (HH_0 C == M)
assert (HH_1 C == 0)
assert (HH_2 C == 0)
assert (HH_3 C == 0)
assert (HH_4 C == 0)
"

document { (resolution, Module),
     TT "resolution M", " -- produces a projective resolution of the 
     module ", TT "M", ".",
     PARA,
     "If the computation is interrupted after the skeleton has been
     successfully computed, then the partially completed
     resolution is available as ", TT "M.resolution", ".  The computation 
     can be continued with ", TT "resolution M", ".",
     PARA,
     "If the user has a chain complex in hand which is known to be a
     projective resolution of ", TT "M", ", then it can be installed
     with ", TT "M.resolution = C", ".",
     PARA,
     "For an abbreviation, use ", TO "res", "."
     }
document { (resolution, Matrix),
     TT "resolution f", " -- when ", TT "f", " is a module homomorphism, produces a
     chain map from a resolution of the source of ", TT "f", " to a resolution of the
     target of ", TT "f", ".",
     EXAMPLE {
	  "R = ZZ/101[x,y];",
      	  "m = ideal vars R",
      	  "resolution map(m/m^3, m^2/m^4)"
	  }
     }


document { (resolution, Ideal),
     TT "resolution I", " -- produces a projective resolution of the 
     module ", TT "R/I", " if ", TT "I", " is an ideal in the ring ", TT "R", "."
     }

TEST "
S = ZZ/101[t_1 .. t_9,u_1 .. u_9]
m = matrix pack (3,toList (t_1 .. t_9))			  -- 3 by 3
n = matrix pack (3,toList (u_1 .. u_9))			  -- 3 by 3
j = flatten (m * n - n * m)
M = cokernel j
C = res(M, LengthLimit => 2, DegreeLimit => 1)
-- assert( rank C_2 == 2 )
C = res(M, LengthLimit => 2, DegreeLimit => 2)
-- assert( rank C_2 == 42 )
C = res(M, LengthLimit => 2, DegreeLimit => 3)
-- assert( rank C_2 == 83 )
C = res(M, LengthLimit => 2, DegreeLimit => 4)
--- assert( rank C_2 == 90 )
"

document { status,
     HEADLINE "status of a resolution computation",
     TT "status C", " -- displays the status of the computation of a
     chain complex C constructed by ", TO "resolution", ".  The display has
     the same shape as the display produced by ", TO "betti", ", but
     the number(s) displayed in each degree differ.",
     PARA,
     "Options:",
     MENU {
	  {TO TotalPairs, " -- display the total number of S-pairs, default value ",
	       toString (options status).TotalPairs },
	  {TO PairsRemaining, " -- display the number of S-pairs remaining, default value ",
	       toString (options status).PairsRemaining},
	  {TO Monomials, " -- display the number of monomials, default value ",
	       toString (options status).Monomials}
	  }
     }
document { TotalPairs,
     TT "TotalPairs", " -- an option for ", TO "status", " which specifies
     whether to display the total number of S-pairs."
     }
document { PairsRemaining,
     TT "PairsRemaining", " -- an option for ", TO "status", " which specifies
     whether to display number of S-pairs remaining."
     }
document { Monomials,
     TT "Monomials", " -- an option for ", TO "status", " which specifies
     whether to display the number of monomials."
     }

document { radical, 
  TT "radical I", " -- the radical of the ideal I",
  BR,NOINDENT,
  TT "radical(I,options)", " -- some options are allowed as well",
  PARA,
  "If I is an ideal in an affine ring (i.e. a quotient of a polynomial 
  ring over a field), and if the characteristic of this field is
  large enough (see below), then this routine yields the radical of
  the ideal I.",
  PARA,
  "The method used is the Eisenbud-Huneke-Vasconcelos algorithm.
  See their paper in invent. math. 1993 for more details on the
  algorithm.",
  PARA,
  "For an example, see ", TO "component example", ".",
  PARA,
  "The algorithms used generally require that the characteristic of the
  ground field is larger than the degree of each primary component.  In 
  practice, this means that if the characteristic is something like 32003,
  rather than e.g. 5, the methods used will produce the radical of I.  Of
  course, you may do the computation over QQ, but it will often run much
  slower.  In general, this routine still needs to be tuned for speed.",
  SEEALSO {"top", "removeLowestDimension", "saturate", "quotient"}
  }

document { CompleteIntersection,
     TT "CompleteIntersection => J", " -- an option to ", TO "radical", " 
     which indicates that the ideal I provided by the user is unmixed,
     and that J is an ideal in I which is a complete intersection of
     the same codimension.",
     PARA,
     "Providing this option allows a separate often faster
     algorithm to be used to compute the radical.  This option
     should only be used if J is nice in some way.  For example,
     if J is randomly generated, but I is relatively sparse, 
     then this will most likely run slower than just giving the
     ", TO "Unmixed", " option."
     }

document { Unmixed,
     TT "Unmixed => true", " -- an option to ", TO "radical", " which asserts
     that the ideal provided by the user is known to be unmixed.",
     PARA,
     "An ideal is said to be unmixed if all associated primes of R/I
     have the same dimension.  In this case the algorithm tends to be much faster."
     }

document { top,
     TT "top I", " -- yields the intersection of top dimensional primary
     components of the module or ideal I.",
     PARA,
     "For an example, ", SEEALSO "component example",
     PARA,
     "If I is a submodule of (a quotient module) M, then a possibly larger
     submodule of M is returned.  The method used is that of
     Eisenbud-Huneke-Vasconcelos, in their 1993 Inventiones Mathematicae
     paper.  For a very brief description of the method used, see ", TO
     "top-method", ".",
     SEEALSO {"removeLowestDimension", "saturate", "quotient", "radical"}
     }

document { removeLowestDimension,
     TT "removeLowestDimension", " I -- I an ideal or submodule of a free module.
     Yields the intersection of the primary
     components of I, excepting those of lowest dimension (and thus returns the
     ambient free module of I (or unit ideal), if I is pure dimensional).",
     PARA,
     "For an example, ",SEEALSO "component example",
     PARA,
     "Computes one free resolution, and some homology groups, but no
     projections or determinants are used.",
     "For a very brief description of the method used, see ", TO "top-method", ".",
     SEEALSO {"top", "saturate", "quotient", "radical", "decompose"}
     }

document { factor,
     TT "factor x", " -- factors x.",
     PARA,
     "The result is a ", TO "Product", " each of whose factors is a 
     ", TO "Power", " whose base is one of the factors found and whose
     exponent is an integer.",
     EXAMPLE {
	  "y = (2^15-4)/(2^15-5)",
      	  "x = factor y",
      	  "value x",
	  },
     "We may ", TO "peek", " inside ", TT "x", " to a high depth to see
     its true structure as ", TO "Expression", ".",
     EXAMPLE "peek2(x,100)",
     PARA,
     "For small integers factorization is done by trial division.  Eventually
     we will have code for large integers.  For multivariate polynomials the
     factorization is done with code of Michael Messollen (see 
     ", TO "Factorization and characteristic sets library", ").  For univariate
     polynomials the factorization is in turn done with code of 
     Gert-Martin Greuel and Ruediger Stobbe (see ", TO "Factory library", ").",
     EXAMPLE {
	  "R = ZZ/101[u]",
      	  "factor (u^3-1)",
	  },
     "The constant term is provided as the last factor.",
     EXAMPLE {
	  "F = frac(ZZ/101[t])",
      	  "factor ((t^3-1)/(t^3+1))",
	  },
     "The code for factoring in a fraction field is easy to read:",
     EXAMPLE "code(factor,F)"
     }

document { pseudoRemainder,
     TT "pseudoRemainder(f,g)", " -- computes the pseudo-remainder for
     f divided by g.",
     PARA,
     "This is an internal experimental routine."
     }

document { irreducibleCharacteristicSeries,
     TT "irreducibleCharacteristicSeries I", " -- computes the irreducible
     characteristic series of ideal I.",
     PARA,
     "This is an internal routine used by ", TO "decompose", "."
     }

document { topCoefficients,
     TT "topCoefficients m", " -- for a matrix m, for each column, returns
     the coefficients of the highest power of the variable with the lowest
     index.",
     PARA,
     "Beware: the greatest variable is usually the first variable.",
     PARA,
     "The value returned is a list ", TT "{monoms, coeff}", ".
     Let x_i be the smallest index variable that occurs in the
     j-th column of ", TT "m", ". Then the j-th column of ", TT "coeff", "
     contains the (vector) coefficient of the highest power of this
     variable, and the j-th element of ", TT "monoms", " is the highest power
     x_i^n."
     }

document { decompose,
     TT "decompose I", " -- compute the ideals of the irreducible
     components of the subvariety defined by the ideal I.",
     PARA,
     "This code uses ", TO "irreducibleCharacteristicSeries", ".  See also
     ", TO "pseudoRemainder", ".",
     PARA,
     "At the moment, cases that involve factorization over extensions of
     prime fields are not handled, and a warning message is issued.  The
     message is: 'Factorisation over algebraic function field required!'.
     The user should take this as an indication that the factorization is
     not complete."
     }

TEST ///
R = ZZ[x,y,z]
f = (x - 2^32 * y) * (x + 2^33 * z - 77 * y)
d = factor f
assert( #d == 3 and value d == f )
///

TEST ///
R=ZZ/32003[a..h]
I=ideal(-b*d^2*f*h^2+a*b*d*g*h^2,
	-a*b*d^2*e^2+c^2*g^2*h^2,
	-d^2*f*g^3+a*b*d*e^2*h)
dec = decompose I
assert(dec#-1 == ideal ( d*f-a*g,  g^4-b*e^2*h, a*d^2*g^2-c^2*h^3,
 	  a*b*d^2*e^2-c^2*g^2*h^2, a^2*b*d*e^2-c^2*f*g*h^2, 
	  a^3*b*e^2-c^2*f^2*h^2 ))
assert(dec == {
	  ideal(c,d),
	  ideal(h,d),
	  ideal(g,d),
	  ideal(h,f,a),
	  ideal(c,f,a),
	  ideal(g,f,a),
	  ideal(h,g,a),
	  ideal(c,f,b),
	  ideal(g,b),
	  ideal(h,f,b),
	  ideal(h,e,f),
	  ideal(g,e,f),
	  ideal(h,g,e),
	  ideal(-d*f+a*g,g^4-b*e^2*h,-d^3*f*g+c^2*h^3,-a^3*b*e^2+c^2*f^2*h^2,-a^2*b*d*e^2+c^2*f*g*h^2,-a*b*d^2*e^2+c^2*g^2*h^2)
	  }
     )
///

TEST ///
    -- permanents!
    R = ZZ/32003[r,s,t,u,v,w,x,y,z]
    I = ideal( r*v+s*u, r*w+t*u, s*w+t*v, r*y+s*x, r*z+t*x, s*z+t*y,
	u*y+v*x, u*z+w*x, v*z+w*y)
    time D = decompose I 
			-- used 130.74 seconds
			-- used 127.85 seconds
		        -- used 102.09 seconds
     			-- used 43.06 seconds (Messollen speed up!)
			-- used 41.93 seconds
			-- used 6.87 seconds, cygnus32
     			-- used 5.19 seconds, linux
			-- 82 seconds in Singular
    assert(D == {
	      ideal(u,r,y,x,z,t*v+s*w),
	      ideal(z,x,y,w,u,v),
	      ideal(v,u,s,w,y,t*x+r*z),
	      ideal(v,s,y,x,z,t*u+r*w),
	      ideal(x,y,r,s,u,v),
	      ideal(v,u,r,w,x,t*y+s*z),
	      ideal(v,s,r,t,y,w*x+u*z),
	      ideal(z,x,y,t,r,s),
	      ideal(u,s,r,t,x,w*y+v*z),
	      ideal(z,x,w,t,r,u),
	      ideal(s,r,t,w,z,v*x+u*y),
	      ideal(v,u,t,w,z,s*x+r*y),
	      ideal(z,y,w,t,s,v),
	      ideal(w,t,r,s,u,v),
	      ideal(t,w,y,x,z,s*u+r*v)
	      })
///

TEST "
     R = ZZ/31991[x,y,z];
     ivd = decompose ideal (x^3-y^2,x^3-z^2,y^3-z^2);
     assert( #ivd ===  5 )
"

TEST "
     R = ZZ/31991[x,y,z]
     I = ideal (x,y)
     J = ideal (y-1,z-1)
     K = intersect(I,J)
     ivd = decompose K
     assert( #ivd == 2 )
"

TEST "
     -- from Wang's paper, example 8.1
     R = ZZ/5[x_1 .. x_4]
     I = ideal (
	       -3*x_3*x_4 + x_2^2 - 2*x_1 + 2,
	       -3*x_1^2*x_4 - 4*x_2*x_3 - 6*x_1*x_3 + 2*x_2^2 + 3*x_1*x_2,
	       -3*x_3^2*x_4 - x_1*x_4 + x_2^2*x_3 + x_2
	       )
     ivd = decompose I
     assert( #ivd === 2 )
"

TEST "
     -- This is a case where P1 factors.
     R = ZZ/109[x,y,z]
     I = ideal ((x-1)^2-(x-1)-3)
     J = ideal (y-1,z^2-z-3)
     P1 = ideal (x^2-x-3,y^2-y-3,z-13)
     P2 = ideal (x-13,y-55,z-12)
     K = intersect(I,J,P1,P2)
     ivd = decompose K
     "

TEST "
R = ZZ/31991[x,y]
assert( (x^2-10748*y*x+y^2)*(y^2+x^2)*(x^2+10748*y*x+y^2) == x^6 + y^6 )
assert ( # factor (x^6 + y^6) == 4 )
"

document { integrate,
     TT "integrate(f,a,b)", " -- integrate f from a to b numerically, using
     Gaussian quadrature.",
     EXAMPLE "integrate(sin,0,pi)"
     }

document { getWWW,
     TT "getWWW URL", " -- obtain the contents of the web page addressed
     by ", TT "URL", " from an http server.",
     BR,NOINDENT,
     TT "getWWW(URL,TEXT)", " -- obtain the contents of the web page addressed
     by ", TT "URL", " from an http server, using the POST method, provided 
     with ", TT "TEXT", ".",
     PARA,
     "This doesn't work under Solaris because Sun doesn't provide sockets
     or name service to statically linked programs like this one.",
     PARA,
     "Accessing a secure web site (whose URL begins with ", TT "https:", ")
     depends on your having installed ", TT "openssl", " on your system."
     }

document { browse,
     TT "browse x", " -- provides an interactive mechanism which allows the user
     to explore the hash table or list ", TT "x", ".",
     PARA,
     "A menu of numbered items is presented to the user which allow the user to
     inspect the ", TO "class", " or ", TO "parent", " of ", TT "x", ".  For a
     hash table, the keys are presented so the user can visit the 
     corresponding values, and for a list, the entries are presented so the user 
     can visit them.  One of the menu items allows the user to go back to 
     re-examine the previous item."
     }

document { showUserStructure,
     TT "showUserStructure", " -- a command which displays the structure of 
     types defined by the user and assigned to global variables.",
     PARA,
     "Each such class is displayed to the right of its parent.",
     PARA,
     "A type is an instance of the class ", TO "Type", ".",
     EXAMPLE {
	  "X = new Type of List",
	  "Y = new Type of X",
	  "Z = new Type of X",
	  "showUserStructure",
	  },
     SEEALSO { "showStructure", "parent"}
     }


document { showStructure,
     TT "showStructure", " -- a command which displays the structure of types
     assigned to global variables.",
     BR, NOINDENT,
     TT "showStructure (X,Y,...)", " -- a command which displays the structure 
     of the types specified.",
     PARA,
     "Each such type is displayed to the right of its parent.",
     PARA,
     "A type is an instance ", TO "Type", ".",
     EXAMPLE {
	  "showStructure {List, Array, Sequence, MutableHashTable, ZZ, Ring, Monoid}",
	  },
     SEEALSO { "showUserStructure", "parent"}
     }

document { symbol "..",
     TT "m .. n", " -- produces a sequence of integers in the range from m to 
     n inclusive. If n is less than m then the result is an empty sequence.",
     PARA,
     EXAMPLE {
	  "1..5",
      	  "{1..5}",
      	  "toList(1..5)"
	  },
     "The most confusing thing about this operator is that it is not a syntactic
     construction, and so the resulting sequences do not splice themselves into
     enclosing lists, as in each of the following examples.  Use ", TO "splice", "
     to fix that.",
     PARA,
     EXAMPLE {
      	  "{10..10}",
      	  "{10..8}",
      	  "{3..5,8..10}",
      	  "splice {3..5,8..10}",
	  },
     PARA,
     NOINDENT,
     TT "a .. i", " -- produces a sequence of symbols for use as 
     variables in polynomial rings.",
     EXAMPLE "a .. i",
     PARA,
     TT "x_0 .. x_9", " -- produces a sequence of indexed variables for use in
     polynomial rings.",
     EXAMPLE {
	  "x_0 .. x_9",
      	  "x_(t_0) .. x_(t_5)",
      	  "x_a .. x_e",
	  },
     PARA,
     NOINDENT,
     "This operator can be used with sequences or lists to produce rectangular
     intervals.",
     EXAMPLE {
	  "(0,0)..(1,3)",
      	  "p_(0,a) .. p_(1,c)",
	  },
     }

document { (cohomology, ZZ, Module),
     TT "HH^i(M)", " -- computes the i-th local cohomology of ", TT "M", " with
     respect to the maximal ideal generated by the variables of the ring.",
     PARA,
     EXAMPLE {
	  "R = QQ[x,y];",
	  "HH^2 (R^{-3})",
	  "HH^2 (R^{-4})"
	  }
     }

document { Variety, HEADLINE "the class of all algebraic varieties", SEEALSO "CoherentSheaf" }

document { AffineVariety, HEADLINE "the class of all affine varieties",
     "To create an affine variety, use ", TO "Spec", ".",
     EXAMPLE { "Spec(QQ[x,y])" }
     }

document { ProjectiveVariety, HEADLINE "the class of all projective varieties",
     "To create a projective variety, use ", TO "Proj", ".",
     EXAMPLE { "Proj(QQ[x,y])" }
     }

document { Spec,
     TT "Spec R", " -- create an affine variety or scheme from the ring ", TT "R", ".",
     EXAMPLE { "R = QQ[x,y];", "Spec R" }
     }

document { Proj,
     TT "Proj R", " -- create a projective variety or scheme from the graded ring ", TT "R", ".",
     EXAMPLE { "R = QQ[x,y];", "Proj R" }
     }

document { CoherentSheaf, HEADLINE "the class of all coherent sheaves" }
document { sheaf, HEADLINE "make a sheaf" }
document { (sheaf, Module, Variety),
     TT "sheaf(M,X)", " -- produce the coherent sheaf on the variety ", TT "X", " corresponding
     to the module ", TT "M", ".",
     PARA,
     "If ", TT "X", " is the affine variety ", TT "Spec R", ", then ", TT "M", " should be an ", TT "R", "-module.  If ", TT "X", " is 
     the projective variety ", TT "Proj R", ", then ", TT "M", " should be a homogeneous ", TT "R", "-module."
     }

document { (sheaf, Module),
     TT "sheaf M", " -- produce the coherent sheaf on a projective variety ", TT "X", "
     corresponding to a homegeneous module ", TT "M", "."
     }

document { (symbol ~, Module),
     TT "M~", " -- produce the coherent sheaf on a projective variety ", TT "X", "
     corresponding to a homegeneous module ", TT "M", ".",
     PARA,
     SEEALSO "CoherentSheaf"
     }

document { (sheaf, Ring), 
     TT "sheaf R", " -- produce the structure sheaf on the projective variety ", TT "Proj R", "."
     }

document { (symbol ~, Ring), HEADLINE "structure sheaf",
     TT "R~", " -- produce the structure sheaf on the projective variety ", TT "Proj R", ".",
     EXAMPLE { "R = QQ[x,y,z];", "R~", "variety oo" }
     }

document { (variety, CoherentSheaf),
     TT "variety F", " -- produce the variety over which a coherent sheaf is defined.",
     PARA,
     EXAMPLE {
	  "X = Proj(QQ[x,y,z])",
	  "OO_X(3)",
	  "variety oo"
	  }
     }
document { variety, HEADLINE "get the variety" }
document { (ring, CoherentSheaf),
     TT "ring F", " -- produce the coordinate ring of the variety on which a coherent sheaf
     ", TT "F", " is defined."
     }

document { (module, CoherentSheaf),
     TT "module F", " -- produce the module from which the coherent sheaf ", TT "F", " was defined.",
     PARA,
     EXAMPLE {
	  "X = Proj(QQ[x,y,z])",
	  "F = OO_X(3)",
	  "module F",
	  "degrees oo",
	  }
     }

document { (symbol ++, CoherentSheaf, CoherentSheaf),
     TT "F ++ G", " -- direct sum of coherent sheaves."
     }

document { (symbol **, CoherentSheaf, CoherentSheaf),
     TT "F ** G", " -- tensor product of coherent sheaves."
     }

document { (symbol " ", CoherentSheaf, ZZ),
     TT "F(n)", " -- twist a coherent sheaf F on a projective variety by
     the n-th power of the hyperplane line bundle.",
     PARA,
     EXAMPLE {
	  "X = Proj(QQ[x,y,z])",
	  "F = OO_X",
	  "G = F(3)",
	  "module G",
	  "degrees oo",
	  }
     }

document { (symbol /, CoherentSheaf, CoherentSheaf),
     TT "F / G", " -- quotient of coherent sheaves.",
     PARA,
     SEEALSO "CoherentSheaf"
     }


document { (codim, CoherentSheaf), HEADLINE "codimension of support" }
document { (rank, CoherentSheaf) }
document { (exteriorPower, ZZ, CoherentSheaf),
     TT "exteriorPower(i,F)", " -- calculate the ", TT "i", "-th exterior power of a coherent sheaf
     ", TT "F", ".",
     }

document { (degrees, CoherentSheaf),
     TT "degrees F", " -- produce a list of the degrees of the generators of the module
     defining a coherent sheaf ", TT "F", "."
     }

document { (cohomology, ZZ, CoherentSheaf),
     TT "HH^i(F)", " -- for a coherent sheaf F on a projective variety X, computes
     the direct sum over at least the natural numbers ", TT "n", ", of the
     ", TT "i", "-th cohomology groups of F(n).",
     BR,
     NOINDENT,
     TT "HH^i(F, Degree=>e)", " -- same as above, but ", TT "n", " ranges over at least
     the integers at least as large as ", TT "e", ".",
     PARA,
     EXAMPLE {
	  "R = QQ[a,b,c,d]/(a^4+b^4+c^4+d^4);",
	  "X = Proj R",
	  "HH^1(cotangentSheaf X)",
	  "hilbertFunction(0,oo)"
	  }
     }

document { OO,
     TT "OO_X", " -- produce the structure sheaf on a variety ", TT "X", "."
     }

document { (cotangentSheaf, ProjectiveVariety),
     TT "cotangentSheaf X", " -- calculate the cotangent sheaf of a variety ", TT "X", "."
     }
document { cotangentSheaf, HEADLINE "make a cotangent sheaf" }
document { (cotangentSheaf, ZZ, ProjectiveVariety),
     TT "cotangentSheaf(p,X)", " -- calculate the ", TT "p", "-th exterior power of
     the cotangent sheaf of a variety ", TT "X", "."
     }

document { Options,
     TT "Options", " -- an option used with ", TO "method", " to specify
     names of optional arguments and their default values.",
     PARA,
     NOINDENT,
     TT "f = method(Options => w)", " -- creates a method which accepts
     optional arguments.  Here 'w' is a list ", TT "{A=>a,B=>b,...}", " of
     optional argument names A,B,... and corresponding default values a,b,...",
     PARA,
     "The methods installed for this method function should be written in
     the form ", TT "opts -> args -> (...)", ".  The argument ", TT "args", "
     will be assigned a hash table of type ", TO "OptionTable", " containing 
     the optional argument names and their values.  The default table can
     be recovered with the function ", TO "options", ".",
     EXAMPLE {
	  "f = method(Options => {Slope => 1, Intercept => 1})",
      	  "f RR := o -> x -> o.Slope * x + o.Intercept",
      	  "f(5.,Slope=>100)",
	  "options f",
	  },
     SEEALSO "method"
     }
