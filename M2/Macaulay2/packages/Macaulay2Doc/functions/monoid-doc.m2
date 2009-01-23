-- -*- coding: utf-8 -*-
document {
     Key => {monoid,(monoid, Array),(monoid, List),(monoid, PolynomialRing),(monoid, QuotientRing), [monoid,DegreeRank], [monoid,Heft],
	  [monoid,Inverses],[monoid,MonomialSize],VariableBaseName,[monoid,VariableBaseName],
	  [monoid,WeylAlgebra],[monoid, Weights],[monoid, MonomialOrder],[monoid, Variables],[monoid,DegreeLift],[monoid,DegreeMap],
	  [monoid,Degrees], [monoid,Local], SkewCommutative,[monoid,SkewCommutative],[monoid, Global], [monoid, Join]
	  },
     Headline => "make or retrieve a monoid",
     Usage => "monoid [a,b,c,...]",
     Inputs => {
	  { TT "[a,b,c,...]", ", an array of variables, as well as optional arguments.  Alternatively, specifying a list
	       {a,b,c,...} produces a monoid with a local ordering, by inserting the option ", TT "Local => true" },
	  DegreeRank => ZZ => {"the length of each multidegree"},
	  Degrees => List => {"a list of degrees or multidegrees of the variables.  Each degree is an integers, and each multidegree is a list of integers.
	       Degrees will be converted into multidegrees of length 1."},
	  Inverses => Boolean => {"whether negative exponents will be allowed, making the monoid into a group"},
	  Global => Boolean => {"whether monoid rings based on this monoid are to be global rings.
	       If set to ", TO "true", ", and the option ", TT "Inverses=>true", " is
	       not specified, then an error is signalled if any of the variables are not greater
	       than 1 in the monomial ordering, as required by the standard theory of Gröbner bases."
	       },
	  Local => Boolean => {"whether this ring is to be a local ring; implies ", TT "Global => false"},
	  MonomialOrder => List => {"an option for specifying the monomial ordering, see ", TO "MonomialOrder"},
	  MonomialSize => ZZ => {"the minimum number of bits to be used for storing each exponent in a monomial.  The exponents are stored as signed binary numbers, so
	       ", TT "n", " bits allows an exponent as large as 2", SUP "n-1", "-1.  Useful values are 8, 16, and 32."},
	  SkewCommutative => {"specifies whether some of the variables skew-commute with each other.  The value ", TT "true", " indicates that all of the
	       variables skew-commute.  Otherwise, the value of the option should be a list of variables, variables names, or integers that will be interpreted
	       as indices of the variables."},
	  Variables => {"a list of variables names, or the number of variables to be made.  This option is useful for those situations when one doesn't care about the
     	       names of the variables in a ring or monoid, or when one is creating a tensor product ring, symmetric algebra, or other ring, and one wants control
     	       over the names of the ring variables."},
	  VariableBaseName => Symbol => {"a symbol ", TT "x", " to be used as the base for constructing a list of subscripted variable names of class ", TO "IndexedVariable", ",
	       of the form ", TT "x_0, ..., x_(n-1)", "."},
	  -- VariableOrder => {},
	  Weights => List => {"a list of integers, or a list of lists of integers, that specify weights for the variables.  The orderings by these weight
	       vectors is prepended to the list of orderings provided by the ", TO "MonomialOrder", " option."},
	  WeylAlgebra => List => {"a list of options of the form ", TT "x=>dx", ", which specifies that ", TT "dx", " plays the role of the derivative
	       with respect to ", TT "x", " in the resulting Weyl algebra when this monoid is made into a polynomial ring."},
	  Heft => List => {"a list of integers (called a heft vector): its dot product (presumably positive) with the degrees of the 
	       variables will be used as a computational aid
	       in certain routines.  If no value for this option is specified, one will be computed for you, and thus there is no need to
	       provide one unless the time spent computing one is onerous; if no heft vector exists, certain
	       computations will not be supported, and others may run more slowly."
	       },
	  Join => Boolean => {
	       "whether the degrees in a new monoid ring based on this monoid will be obtained by joining the degrees in the monoid
	       with the degrees in the coefficient ring; default: ", TO "true"
	       },
	  DegreeMap => Function => {
	       "the degree map, to be used if ", TT "Join => false", " is given:
	       a (linear) function from the multidegrees of the (future) coefficient ring to the multidegrees
	       of the monoid ring (polynomial ring) made from it with the monoid created here,
	       to be used in determining homogeneity and in determining degrees in tensor products.
	       The default is the ", TO "identity", "."
	       },
	  DegreeLift => Function => {
	       "the degree lift function: a (partial) inverse of the degree map, giving an
	       error when lifting is not possible.  If the degree map is the identity, then by default the identity map
	       will be provided."
	       }
	  },
     Outputs => {
	  {"a new monoid"}
	  },
     PARA {
	  "The function ", TO "monoid", " is called whenever a polynomial ring is created, see ", TO (symbol SPACE, Ring, Array), ",
	  or when a local polynomial ring is made, see ", TO (symbol SPACE, Ring, List), ".
	  Some of the options provided
	  when making a monoid don't take effect until the monoid is made into a polynomial ring."
	  },
     "Let's make a free ordered commutative monoid on the variables ", TT "a,b,c", ", with degrees 2, 3, and 4, respectively.",
     EXAMPLE lines ///
          M = monoid [a,b,c,Degrees=>{2,3,4}]
	  degrees M
	  M_0 * M_1^6
     ///,
     "Use ", TO "use", " to arrange for the variables to be assigned their values in the monoid.",
     EXAMPLE lines ///
     	  a
     	  use M
	  a * b^6
     ///,
     "The options used when the monoid was created can be recovered with ", TO "options", ".",
     EXAMPLE lines ///
     	  options M
     ///,
     "The variables listed may be symbols or indexed variables. The values assigned to these variables are the corresponding monoid generators.  
     The function ", TO "baseName", " may be used to recover the original symbol or indexed variable.",
     PARA{
     	  "The ", TO "Heft", " option is used in particular by ", TO (Ext,Module,Module), ".",
     	  },
     EXAMPLE lines ///
	  R = ZZ[x,y, Degrees => {-1,-2}, Heft => {-1}]
     	  degree \ gens R
	  transpose vars R
     ///,
     "In this example we make a Weyl algebra.",
     EXAMPLE lines ///
         R = ZZ/101[x,dx,y,dy,WeylAlgebra => {x=>dx, y=>dy}]
	 dx*x
      	 dx*x^10
      	 dx*y^10
     ///,
     "In this example we make a skew commutative ring.",
     EXAMPLE lines ///
	  R = ZZ[x,y,z,SkewCommutative=>{x,y}]
      	  x*y
      	  y*x
	  x*z-z*x
     ///,
     "By default, (multi)degrees are concatenated when forming polynomial rings over polynomial
     rings, as can be seen by examining the corresponding flattened monoid, which displays information
     about all of the variables.",
     EXAMPLE lines ///
     QQ[x][y]
     oo.FlatMonoid
     QQ[x][y][z]
     oo.FlatMonoid
     ///,
     "That behavior can be overridden with the ", TO "Join", " option.",
     EXAMPLE lines ///
     QQ[x][y,Join => false]
     oo.FlatMonoid
     ///,
     "A degree map may be provided, and it will be used in computing tensor products.",
     EXAMPLE lines ///
     A = QQ[x];
     B = A[y,Join => false,DegreeMap => x -> 7*x]
     B.FlatMonoid
     degrees A^{-1,-2}
     degrees (B**A^{-1,-2})
     ///,
     "For certain applications, such as lifting matrices, a degree lift function can
     be provided.",
     EXAMPLE {
     	  ///B = A[y,Join => false,DegreeMap => x -> 7*x,
     DegreeLift => x -> apply(x, d -> lift(d/7,ZZ))]///,
     	  ///matrix {{x_B}}///,
	  ///degrees oo///,
	  ///lift(matrix {{x_B}},A)///,
	  ///degrees oo///
	  },
     SYNOPSIS (
	  Usage => "monoid R",
	  Inputs => {
	       "R" => Ring
	       },
	  Outputs => {
	       {"the monoid of monomials in the polynomial ring ", TT "R" }
	       },
	  PARA {
	       "If ", TT "R", " is a quotient ring of a polynomial ring ", TT "S", ", then the monoid of ", TT "S", " is returned."
	       },
	  EXAMPLE lines ///
	  R = QQ[a..d, Weights=>{1,2,3,4}]
	  monoid R
	  ///
	  )
     }
