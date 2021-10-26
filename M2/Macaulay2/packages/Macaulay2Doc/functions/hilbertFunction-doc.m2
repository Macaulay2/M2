--- author(s): L. Gold, Dan Grayson

undocumented {}

document { 
     Key => {hilbertFunction,
	  (hilbertFunction,List,Ring),(hilbertFunction,ZZ,Ring),
	  (hilbertFunction,List,Module),(hilbertFunction,ZZ,Module),
	  (hilbertFunction,List,Ideal),(hilbertFunction,ZZ,Ideal),
	  (hilbertFunction,List,ProjectiveVariety),(hilbertFunction,ZZ,ProjectiveVariety),
	  (hilbertFunction,List,CoherentSheaf),(hilbertFunction,ZZ,CoherentSheaf)
	  },
     Headline => "the Hilbert function",
     Usage => "hilbertFunction(d,X)",
     Inputs => {
	  "d" => {"an integer (or a list of integers) specifying a degree (or multidegree)"},
	  "M" => {"a ring, module, ideal, coherent sheaf, or projective variety"}
	  },
     Outputs => {
	  ZZ => {"the dimension of the degree ", TT "d", " part of ", TT "M", ".  For an
	       ideal, the corresponding quotient ring is used.
	       For a projective varieties and coherent sheaves, the functionality is not yet implemented."}
	  },
     PARA {
     	  "In the following example, compare the rank of the source of the basis map to the number provided by ", TO "hilbertFunction", "."
	  },
     EXAMPLE lines ///
     R = QQ[x,y,z, Degrees=>{3:{1,1}}];
     hilbertFunction({3,3}, R)
     basis({3,3},R)
     ///,     
     "The standard meaning of subscripts on functions permits a simpler syntax to be used.",
     EXAMPLE lines ///
     hilbertFunction_{3,3} R
     ///,
     "Here is a singly graded example.",
     EXAMPLE lines ///
     R = QQ[x,y,z];,
     hilbertFunction({3}, R)
     hilbertFunction(3, R)
     ///,
     "Here is an example with a module.",
     EXAMPLE lines ///
     R = QQ[a..d, Degrees=>{4:{1,1}}];
     M = coker matrix {{a,c,d},{c,b,d}}
     hilbertFunction({2,2}, M)
     B = basis({2,2},M)
     numgens source B
     ///,
     "Here is an example with an ideal.",
     EXAMPLE lines ///
     R = QQ[a..f, Degrees=>{6:{1,1}}];
     I = ideal (a*b, c*d, e*f);
     hilbertFunction({2,2}, I)
     S = R/I;
     basis({2,2},S)
     ///,
     Caveat => {
	  "It can be much faster to compute a basis for the desired degree, because hilbertFunction works by
	  expanding the Hilbert series to a sufficiently high order, thus, in effect, computing many values of the
	  Hilbert function simultaneously.  If several values of the Hilbert function are desired, it is best
	  to compute the ones of higher degree first, so the expansion will be done to sufficiently high order
	  at the first attempt, and thus be done just once."
	  },
-* no longer true:
     Caveat => {
     	  "This requires a homogeneous module to compute properly, but
	  will output something if run on a module which is not homogeneous."
	  },
*-
     SeeAlso => {degreesRing, reduceHilbert, poincare, poincareN, hilbertSeries, hilbertPolynomial, numgens, (symbol _, Function, Thing)}
     }
