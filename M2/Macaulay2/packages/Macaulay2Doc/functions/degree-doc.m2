--- status: DRAFT
--- author(s): M. Stillman
--- notes: 

-*
-- TODO
degree(ChainComplexMap)
degree(GradedModuleMap)
degree(Matrix)
*-

document {
     Key => Parenthesize,
     "This class is used internally to implement the parentheses inserted by ", TO "parts", "."
     }

undocumented {
	  (degree, MonomialIdeal),
	  (degree, CoherentSheaf),
	  (degree, Number)
	  }

document { 
     Key => degree,
     "Degree is a common name, meaning different things for different 
     kinds of mathematical objects.  In Macaulay2, there are currently three
     related, yet different notions of degree: ",
     HEADER3 "Degree of polynomials or vectors of such",
	  UL {
	  TO (degree,RingElement),
	  TO (degree,Vector),
	  TO (degree,RingElement,RingElement),
	  TO (degree,ProjectiveHilbertPolynomial)
	  },
	  HEADER3 "Degree of ideals, varieties and modules",
	  UL {
	  TO (degree,Ideal),
	  TO (degree,Ring),
	  TO (degree,Module),
	  TO (degree,ProjectiveVariety)
	  },
	  HEADER3 "Degree of homomorphisms",
	  UL {
	  TO (degree,Matrix),
	  TO (degree,ChainComplexMap),
	  TO (degree,GradedModuleMap)
	  },
     SeeAlso => {degreeLength, degreesRing
	  -- Mike wanted this: , "multigraded polynomial rings"
	  }
     }
document { 
     Key => (degree,ProjectiveVariety),
     Usage => "degree X",
     Inputs => { "X" },
     Outputs => {
	  ZZ => {"the degree of ", TT "X"}
	  },
     EXAMPLE {
	  "S = ZZ/32003[x,y,z];",
	  "I = ideal(x^4-4*x*y*z^2-z^4-y^4);",
	  "R = S/I;",
	  "X = variety I",
	  "degree X"
	  },
     "The degree of a projective variety ", TT "X = V(I) = Proj R", " is the degree
     of the homogeneous coordinate ring ", TT "R = S/I", " of ", TT "X", ".",
     EXAMPLE {
          "degree X == degree I",
	  "degree X == degree R"
	  },
     SeeAlso => {(degree,Ideal),variety, "varieties"}
     }
document { 
     Key => (degree,ProjectiveHilbertPolynomial),
     Usage => "degree f",
     Inputs => {
	  "f" => {"usually returned via ", TO "hilbertPolynomial"}
	  },
     Outputs => {
	  ZZ => "the degree of any graded module having this hilbert polynomial"
	  },
     "This degree is obtained from the Hilbert polynomial ", TT "f", " as follows:
     if ", TT "f = d z^e/e! + lower terms in z", ", then ", TT "d", " is returned.
     This is the lead coefficient of the highest", TT "P^e", " in the ", TO ProjectiveHilbertPolynomial,
     " display.",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "I = ideal(a^3, b^2, a*b*c);",
	  "F = hilbertPolynomial I",
	  "degree F"
	  },
     "The degree of this polynomial may be recovered using ", TO dim, ":",
     EXAMPLE {
	  "dim F"
	  },
     "The dimension as a projective variety is also one less that the Krull dimension of ", TT "R/I",
     EXAMPLE {
	  "(dim I - 1, degree I)"
	  },
     SeeAlso => {hilbertPolynomial}
     }
document { 
     Key => (degree,Ideal),
     Usage => "degree I",
     Inputs => {
	  "I" => "in a polynomial ring or quotient of a polynomial ring"
	  },
     Outputs => {
	  ZZ => {"the degree of the zero set of ", TT "I"}
	  },
     "The degree of an ideal ", TT "I", " in a ring ", TT "S", " is the degree of the module 
     ", TT "S/I", ".  See ", TO (degree,Module), " for more details.",
     EXAMPLE {
	  "S = QQ[a..f];",
	  "I = ideal(a^5, b^5, c^5, d^5, e^5);",
	  "degree I",
	  "degree(S^1/I)",
	  },
     "If the ideal is not homogeneous, then the degree returned is the degree of the
     ideal of initial monomials (which is homogeneous).  If the monomial order is 
     a degree order (the default), this is the same as the degree of the 
     projective closure of the zero set of ", TT "I", ".",
     EXAMPLE {
	  "I = intersect(ideal(a-1,b-1,c-1),ideal(a-2,b-1,c+1),ideal(a-4,b+7,c-3/4));",
	  "degree I"
	  },
     SeeAlso => {dim, codim, genus, genera, hilbertSeries, reduceHilbert, poincare, hilbertPolynomial}
     }
document { 
     Key => (degree,Module),
     Usage => "degree M",
     Inputs => {
	  "M" => "over a polynomial ring or quotient of a polynomial ring, over a field k"
	  },
     Outputs => {
	  ZZ => {"the degree of ", TT "M"}
	  },
     "We assume that ", TT "M", " is a graded (homogeneous) module over a 
     polynomal ring or a quotient of a polynomial ring with all degrees of variables and heft vector equal to ", TT "{1}", ",
     over a field ", TT "k", ".",
     PARA{
	  "If ", TT "M", " is finite dimensional over ", TT "k", ", the degree
	  of ", TT "M", " is its dimension over ", TT "k", ".  Otherwise, the
	  degree of ", TT "M", " is the multiplicity of ", TT "M", ", i.e., the
	  integer ", TT "d", " such that the Hilbert polynomial of ", TT "M", "
	  has the form ", TT "z |--> d z^e/e! + lower terms in z.",
	  },
     EXAMPLE lines ///
	  R = ZZ/101[t,x,y,z];
	  degree (R^1 / (ideal vars R)^6)
	  degree minors_2 matrix {{t,x,y},{x,y,z}}
	  ///,
     PARA {
     	  "The algorithm computes the ", TO "poincare", " polynomial of ", TT "M", ",
	  divides it by ", TT "1-T", " 
	  as often as possible, then evaluates it at ", TT "T=1", ".
	  When the module has finite length,                                                           
 	  the result is the Hilbert series evaluated
 	  at 1, that is the dimension over the ground field, which for a graded (homogeneous)
	  is the same as the length."
	  },
     Caveat => {
	  "If the base ring is ", TO "ZZ", ",
	  or the module is not homogeneous, it is likely that the answer is not what
	  you would expect.  Similarly, if the degrees of the variables
	  are not all ", TT "{1}", ", or the heft vector isn't ",TT "{1}", ", then the answer is harder to interpret.  See ", TO "heft vectors", " and
	  ", TO "multidegree", "."
	  },
     SeeAlso => {hilbertPolynomial, isHomogeneous}
     }

doc ///
Key
  (length, Module)
Headline
  Computes the length of a module
Usage
  l = length M
Inputs
  M: Module
Outputs
  l: ZZ
    the length of M
Description
  Text
    If M is a graded module over a singly graded polynomal ring or a quotient of a
    polynomial ring over a field k then length is the same as the degree.

    If M is over a local ring then length is computed by summing the output of
    the Hilbert-Samuel function until it vanishes. Note that in this case the
    @TO "LocalRings :: LocalRings"@ package must be loaded first.
Consequences
  Item
    In the local case, the length of the module is stored in M.cache.length.
Caveat
  In the local case, the input is assumed to have finite length.
SeeAlso
  (degree, Module)
///

document { 
     Key => (degree,Ring),
     Usage => "degree R",
     Inputs => {
	  "R" => "a quotient of a polynomial ring"
	  },
     Outputs => {
	  ZZ => {"the degree of ",  TT "R"}
	  },
     "If ", TT "R = S/I", ", where ", TT "S", " is a polynomial ring, then the degree of ", TT "R", " is
     by definition the degree of ", TT "I", ", or the degree of the ", TT "S", "-module ", TT "R", ".",
     "  See ", TO (degree,Module), " for more details.",
     EXAMPLE {
	  "R = QQ[a..d]/(a*d-b*c, b^2-a*c, c^2-b*d)",
	  "degree R",
	  },
     SeeAlso => {(degree,Module)}
     }
document { 
     Key => {(degree,RingElement),(degree,Vector)},
     Usage => "degree f",
     Inputs => {
	  "f" => {"a ", TO2(RingElement, "ring element"), 
	       " or ", TO2(Vector, "vector")}
	  },
     Outputs => {
	  List => {"the degree or multidegree of ", TT "f"}
	  },
     "In Macaulay2, the degree of a polynomial is a list of integers.
     This is to accommodate polynomial rings having multigradings.  The 
     usual situation is when the ring has the usual grading: each variable has
     length 1.",
     EXAMPLE {
	  "R = QQ[a..d];",
	  "degree (a^3-b-1)^2",
	  },
     "When not dealing with multigraded rings, obtaining the degree as a number
     is generally more convenient:",
     EXAMPLE {
	  "first degree (a^3-b-1)^2"
	  },
     EXAMPLE {
	  "S = QQ[a..d,Degrees=>{1,2,3,4}];",
	  "first degree (a+b+c^3)"
	  },
     EXAMPLE {
	  "T = QQ[a..d,Degrees=>{{0,1},{1,0},{-1,1},{3,4}}];",
	  "degree c",
	  },
     "In a multigraded ring, the degree of a polynomial whose terms
     have different degrees is perhaps non-intuitive: it is the 
     maximum (in each of the component degree) over each term:",
     EXAMPLE {
	  "degree c^5",
	  "degree d",
	  "degree (c^5+d)"
	  },
     Caveat => {},
     SeeAlso => {isHomogeneous, degreeLength}
     }
document { 
     Key => (degree,RingElement,RingElement),
     Headline => "degree with respect to a variable",
     Usage => "degree(x,f)",
     Inputs => {
	  "x" => {"a variable in the same ring"},
	  "f" => {"in a polynomial ring ", TT "R"}
	  },
     Outputs => {
	  ZZ => {"highest power of ", TT "x", " occurring in ", TT "f"}
	  },
     EXAMPLE {
	  "R = QQ[a..d];",
	  "degree(b, a*b^5+b^7-3*a^10-3)"
	  }
     }

///
R = QQ[a..d,Degrees=>{{0,1},{1,0},{-1,1},{-2,1}}]
I = ideal(a,c)
hf = poincare I
T_0 * oo
use ring oo
hf % (1-T_0)

factor oo
degree I

///

