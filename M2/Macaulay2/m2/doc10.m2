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
document { Resolution,
    TT "Resolution", " -- the class of all resolution computations, as well
    as the key used in a ", TO "ChainComplex", " to store the resolution it
    comes from.",
    PARA,
    "These resolutions are internal engine objects not meant to be examined
    by the user.",
    PARA,
    "Functions dealing with resolutions:",
    MENU {
	 TO "status"
	 }
    }

document { "res",
    "See ", TO "resolution", ", of which ", TT "res", " is a synonym."
    }

document { resolution,
     TT "resolution", " -- a command for producing resolutions.",
     PARA,
     "See one of the following entries.",
     MENU {
	  TO (resolution, Module),
	  TO (resolution, Matrix),
	  TO (resolution, Ideal)
	  }
     }

document { resolution => DegreeLimit,
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
     TT "StopBeforeComputation", " -- keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA,
     "Tells whether to start the computation, with the default value
     being ", TT "true", ".  This can be useful when you want to obtain
     the partially computed resolution contained in an interrupted computation."
     }

document { LengthLimit,
     TT "LengthLimit", " -- a keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA,
     MENU {
	  TO (resolution => LengthLimit),
	  }
     }

document { Algorithm,
     TT "Algorithm", " -- a keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA,
     MENU {
	  TO (resolution => Algorithm),
	  }
     }

document { resolution => LengthLimit,
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
     that information above a specified degree is to be discarded.",
     PARA,
     "See:",
     MENU {
	  TO (resolution => HardDegreeLimit)
	  }
     }

document { resolution => HardDegreeLimit,
     TT "HardDegreeLimit", " -- keyword for an optional argument used with
     ", TO "resolution", ".",
     PARA,
     "The default value is ", TT "{}", ".",
     PARA,
     "Information above the specified degree is discarded."
     }

document { resolution => Algorithm,
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
     specifies the strategy to be used for sorting S-pairs.",
     PARA,
     MENU {
	  TO (resolution => SortStrategy),
	  }
     }

document { resolution => SortStrategy,
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
     "Optional arguments and flags:",
     MENU {
	  (TO (resolution => Algorithm), "             -- which algorithm to use"),
	  (TO (resolution => StopBeforeComputation), " -- whether to stop the computation immediately"),
	  (TO (resolution => DegreeLimit), "           -- compute only up to this degree"),
	  (TO (resolution => HardDegreeLimit), "       -- always compute only up to this degree"),
	  (TO (resolution => SyzygyLimit), "           -- stop when this number of syzygies are obtained"),
	  (TO (resolution => PairLimit), "             -- stop when this number of pairs are handled"),
	  (TO (resolution => LengthLimit), "           -- stop when the resolution reaches this length"),
	  (TO (resolution => SortStrategy), "          -- specify strategy for sorting S-pairs")
	  },
     PARA,
     "For an abbreviation, use ", TO "res", ".",
     SEEALSO {"ChainComplex", "resolution"}
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
     module ", TT "R/I", " if ", TT "I", " is an ideal in the ring ", TT "R", ".",
     SEEALSO "resolution"
     }

TEST "
S = ZZ/101[t_1 .. t_9,u_1 .. u_9]
m = matrix pack (toList (t_1 .. t_9),3)			  -- 3 by 3
n = matrix pack (toList (u_1 .. u_9),3)			  -- 3 by 3
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
     TT "status C", " -- displays the status of the computation of a
     chain complex C constructed by ", TO "resolution", ".  The display has
     the same shape as the display produced by ", TO "betti", ", but
     the number(s) displayed in each degree differ.",
     PARA,
     "Options:",
     MENU {
	  {TO TotalPairs, " -- display the total number of S-pairs, default value ",
	       (options status).TotalPairs },
	  {TO PairsRemaining, " -- display the number of S-pairs remaining, default value ",
	       (options status).PairsRemaining},
	  {TO Monomials, " -- display the number of monomials, default value ",
	       (options status).Monomials}
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
  "Allowable options include",
  MENU {
       TO "Unmixed",
       TO "CompleteIntersection"
       },
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

document { "component example",
     "The following simple example illustrates the use of ", 
     TO "removeLowestDimension", ",", TO "top", ",", TO "radical",
     ", and ", TO "decompose", ".",
     EXAMPLE {
	  "R = ZZ/32003[a..d];",
      	  "I = monomialCurve(R,{1,3,4})",
      	  "J = ideal(a^3,b^3,c^3-d^3)",
      	  "I = intersect(I,J)",
      	  "removeLowestDimension I",
      	  "top I",
      	  "radical I",
      	  "decompose I"
	  },
     }

document { "top-method",
     "If M is a module in a polynomial ring R, then the implementations of ",
     TO "top", " and ", TO "removeLowestDimension", " are based on 
     the following observations:",
     MENU {
	  "codim Ext^d(M,R) >= d, for all d (if the module is non-zero)",
	  "If P is an associated prime of M of codimension d := codim P > codim M,
	  then codim Ext^d(M,R) = d and the annihilator of Ext^d(M,R) is contained
	  in P",
	  "If codim Ext^d(M,R) = d, then there really is an associated prime 
	  of codimension d.",
	  "If M is R/I, then top(I) = ann Ext^c(R/I,R), where c = codim I"
	  }
     }

TEST "
    R = ZZ/32003[a..d]
    I = monomialCurve(R,{1,3,4})
    J = ideal(a^3,b^3,c^3-d^3)
    I = intersect(I,J)
    removeLowestDimension I
    top I
    radical I
    decompose I
"
TEST "
    -- test of removeLowestDimension
    R = ZZ/32003[a,b,c]
    I = ideal(a^2,b^2)
    J = ideal(a^3,b^3,c^3)
    I = intersect(I,J)
    time (I1 = removeLowestDimension I)
    time top I
    time radical I
"

     
TEST "
    -- examples of use of: radical, UnmixedRadical, 
    -- top, removeLowestDimension

    -- example 1: a simple monomial ideal
    R = ZZ/101[a..d]
    I = intersect(ideal(a^2,b^2,c), ideal(a,b^3,c^2))
    time (Irad = radical(I,Unmixed=>true))

    -- example 2: 
    R = ZZ/101[a..d]
    I = intersect(ideal(a^2,b^2,c), ideal(a,d^4), ideal(b^2,c^2,d^2))
    time (Itop = top I)
    time (I1 = removeLowestDimension I)
    time (Irad = radical I)
"

TEST "
R = ZZ/101[quote a..quote d]
I = monomialCurve(R,{1,2,3})
I^2
removeLowestDimension(I^2)
assert(I == 
     radical(I^2)
     )
assert(I == 
     radical(I^2, Unmixed=>true)
     )
assert(
     top (I^2) == I^2
     )
S = R/(a^3, b^3)
I = ideal(0_S)
J = I^2
J1 = top J
J1 == J   
time (radical I)

-- 3 by 3 nilpotent matrices
R = ZZ/101[vars(0..8)]
M = genericMatrix(R,a,3,3)
I = ideal (M^3)
I1 = ideal(I_0,I_1,I_2)
codim I1
radical(I, CompleteIntersection=>I1)
-- radical(I,Unmixed=>true)
-- I1 = removeLowestDimension I
-- I2 = removeLowestDimension I1
"

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
     or name service to statically linked programs like this one."
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
