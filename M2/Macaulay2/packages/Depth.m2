-- -*- coding: utf-8 -*-
--=========================================================================--
--=========================================================================--
--=========================================================================--
{*
Author: Bart Snapp.
This file is in the public domain.
*} 
newPackage(
     "Depth",
     Version => "0.5.3", 
     Date => "September 2010",
     Authors => {
	  {Name => "Bart Snapp", Email => "snapp@math.ohio-state.edu", HomePage => "http://www.math.ohio-state.edu/~snapp/"}
	  },
     Headline => "aids in computations related to depth",
     DebuggingMode => false
     )

-- Bart Snapp is not maintaining this code, so others are welcome to suggest improvements.

--=========================================================================--
     
export{"regularSequenceCheck","isRegularSequence","regularSequence","isCM","Sparseness","Bound","Attempts","Maximal"} 
        
--=========================================================================--

-- All this does is check where the ext modules don't vanish.

depth(Ideal,Module) := ZZ => (I,M) -> (
     AI := (ring I)^1/I;
     for i from 0 to dim ring M do(
	  if Ext^i(AI,M) != 0 then return i); 
     infinity
     )

-----------------------------------------------------------------------------

depth(Module) := ZZ => M -> (
    --depth of a module with respect to the max ideal, via finite proj dim
    --gives error if the ultimate coeficient ring of R = ring M is not a field.
    R := ring M;
    
    if not isCommutative R then error"depth undefined for noncommutative rings";
    
    S := (flattenRing R)_0;
    
    if not isField coefficientRing S then error"input must be a module over an affine ring";
    
    S0 := ring presentation S;
    m := presentation M;
    COK := prune coker(sub(m,S0) | (presentation S ** target m));
    
    numgens S0 - length res COK    
--    depth(ideal gens ring M,M)
     )

-----------------------------------------------------------------------------

depth(Ideal,Ring) := ZZ => (I,A) -> (
     depth(I,module A)
     )

-----------------------------------------------------------------------------

depth(Ideal,QuotientRing) := ZZ => (I,A) -> (
     R := ambient A;
     if isField coefficientRing A and isPolynomialRing R and I == ideal gens A and isHomogeneous ideal A then (
	  d := dim R;
	  d - length res(ideal A, LengthLimit => d)) else 
     depth(I,module A)
     )

-----------------------------------------------------------------------------

depth(Ring) := ZZ => A -> depth(ideal gens A,A)

-----------------------------------------------------------------------------

depth(Ideal,Ideal) := ZZ => (I,A) -> (
     depth(I,module A)
     )

-----------------------------------------------------------------------------

depth(Ideal,PolynomialRing) := ZZ => (I,A) -> (
     if isField coefficientRing A then codim I else depth(I,module A)
     ) -- if we can compute dimensions over ZZ, then we can remove this if-then statement

-----------------------------------------------------------------------------

TEST /// 
A = QQ[x,y,z]/ideal(x^2)
m = ideal vars A
assert(depth(m,A) == 2)
depth(ideal(y),A)
-- we don't compute dimensions over ZZ, for now.
-- A = ZZ[x,y]
-- m = ideal vars A
-- depth(m,A)
///


--=========================================================================--

regularSequenceCheck = method()
regularSequenceCheck(List, Module) := ZZ => (X,M) -> (
     X = splice X;
     for i from 0 to #X-1 do (
     	  f := X_i * id_M;
     	  if not isInjective f
     	  then return i else M = coker f);
     #X)

-----------------------------------------------------------------------------

TEST /// 
A = ZZ/101[x_1..x_4]
assert(regularSequenceCheck({x_1..x_4},A^1)==4)
///

-----------------------------------------------------------------------------

regularSequenceCheck(List, Ring) := ZZ => (X,A) -> (
     regularSequenceCheck(X,A^1)
     )

-----------------------------------------------------------------------------

regularSequenceCheck(Matrix, Module) := ZZ => (X,M) -> (
     regularSequenceCheck(flatten entries X,M)
     )

-----------------------------------------------------------------------------

regularSequenceCheck(Matrix, Ring) := ZZ => (X,A) -> (
     regularSequenceCheck(flatten entries X,A)
     )

--=========================================================================--

isRegularSequence = method()
isRegularSequence(List, Module) := Boolean => (X,M) -> (
     if isHomogeneous matrix{X} and isHomogeneous M then (
	  g := reduceHilbert hilbertSeries M;
     	  f := reduceHilbert hilbertSeries (M/ideal X);
	  if numerator f == 0 then return false;
     	  R := degreesRing M; 
     	  T := R_0;
     	  numerator f * value denominator g == (value denominator f) * product(X,i-> (1-T^(first degree i))) * numerator g
	  ) 
     else 
     regularSequenceCheck(X,M) == #splice(X) and ideal(X)*M != M
     )

-- this routine actually consists of 2 tests. In the homogeneous case,
-- we use an exercise from Eisenbud's book "Commutative algebra with a
-- view toward Algebraic Geometry." See p. 555. Otherwise the routine
-- is rather naive.

-----------------------------------------------------------------------------

--isRegularSequence(Sequence, Module) := Boolean => (X,M) -> isRegularSequence(toList X,M)

-----------------------------------------------------------------------------

isRegularSequence(Matrix, Module) := Boolean => (X,M) -> isRegularSequence(flatten entries X,M)

-----------------------------------------------------------------------------

isRegularSequence(List, Ring) := Boolean => (X,A) -> isRegularSequence(X,A^1)

-----------------------------------------------------------------------------

--isRegularSequence(Sequence, Ring) := Boolean => (X,A) -> isRegularSequence(toList X,A^1)

-----------------------------------------------------------------------------

isRegularSequence(Matrix, Ring) := Boolean => (X,A) -> isRegularSequence(X,A^1)

-----------------------------------------------------------------------------

isRegularSequence(List) := Boolean => X -> isRegularSequence(X,ring(X_0))

-----------------------------------------------------------------------------

isRegularSequence(Matrix) := Boolean => X -> isRegularSequence(X,ring X)

--=========================================================================--

regularSequence = method(Options => {Sparseness => .5, Bound => 1, Attempts => 100, Maximal => true})
regularSequence(Ideal,Ring) := Matrix => opts -> (I,A) -> (
     k := coefficientRing A;
     f := gens I;
     r := numColumns f;
     c := codim I;
     if c == infinity then return map(A^1,A^0,0);
     PHI := 0;
     longestSeq := 0;
     for i from 0 to opts.Attempts do (
	  phi := matrix randomMutableMatrix(r,c,opts.Sparseness,opts.Bound);
	  rcs := regularSequenceCheck(compress(f*phi),A);
	  if rcs == c then return f*phi; 
	  if not opts.Maximal then if rcs > longestSeq then (
	       PHI = phi;
	       longestSeq = rcs;
	       );
	  );
     if PHI == 0 then << "--warning: no maximal regular sequence found" <<endl;
     compress(f*PHI)
     )

TEST /// 
A = ZZ/5051[x, y, z];
I = ideal (x, x*y, y*z);
-- the success of this test depends on the random number generator:
setRandomSeed()
assert(regularSequence(I,A,Bound=>100,Sparseness=>.9) - matrix {{90*y*z-2*x, -71*y*z+38*x}}==0)
///

-----------------------------------------------------------------------------

regularSequence(Ring) := Matrix => opts -> A -> regularSequence(ideal gens A,A)
	       
--=========================================================================--

isCM = method()
isCM(Ring) := Boolean => (A) -> (
     dim(A) == depth(A) -- note we should *not* switch to modules - see depth(Ideal,QuotientRing)
     )

-----------------------------------------------------------------------------

isCM(Module) := Boolean => (M) -> (
     dim(M) == depth(M)
     )

--=========================================================================--
--=========================================================================--

beginDocumentation() -- the start of the documentation

-----------------------------------------------------------------------------

document { 
     Key => Depth,
     Headline => "computations involving regular sequences",
     EM "Depth", " is a package which will hopefully help users 
     make computations involving regular sequences and depth. In particular, we add the functions ", TO (depth,Ideal, Ring), 
     " and ", TO (regularSequence,Ideal,Ring), "."
     }

-----------------------------------------------------------------------------

document {
     Key => {
	  (depth, Ideal, Ring),
	  (depth, Ring),
	  (depth, Ideal, Module),
	  (depth, Module),
	  (depth, Ideal, Ideal),
	  (depth, Ideal, PolynomialRing),
	  (depth, Ideal, QuotientRing)
	  },
     Headline => "computes the depth of a ring",
     Usage => "depth(I,A)",
     Inputs => {
	  "I" => {},
	  "A" => {}
	  },
     Outputs => {
	  ZZ => {"the ", TT "I", "-depth of a ring"}
	  },
          "The function ", TT "depth(I,A)", ", computes the ", TT "I",
"-depth of a ring. In the most general setting, it does this by computing ", 
TT "Ext^i(A^1/I,A)", " and noting where it does not vanish. If the ring in 
question is a polynomial ring over a field, then it merely computes the codimension of ", TT "I", 
". If the ring in question is a quotient of a polynomial ring over a field and we are computing 
the depth over the ideal generated by  ", TO (gens, Ring), ", then it computes the 
difference between the dimension of this polynomial ring and the projective dimension of the 
quotient ring. If the ideal is omitted, then the depth is taken over the ideal generated by ", TO (gens, Ring), ".", 
     EXAMPLE lines ///
     A = QQ[x_1..x_3]/ideal(x_1^2, x_1*x_2);
     depth A
     ///,
     "If ", TT "I", " contains a unit, then ", TT "depth(I,A)", " outputs ", TO "infinity", ".",
     EXAMPLE lines ///
     depth(ideal(1),ZZ)
     ///,
     PARA {
     	  "This symbol is provided by the package ", TO Depth, "."
     	  }
     }	   

-----------------------------------------------------------------------------

document {
     Key => {regularSequenceCheck,  
	  (regularSequenceCheck,List,Module),
  	  (regularSequenceCheck,List,Ring),
  	  (regularSequenceCheck,Matrix,Module),
  	  (regularSequenceCheck,Matrix,Ring)
	  },
     Headline => "how much of a list is regular",
     Usage => "regularSequenceCheck(X,A)",
     Inputs => {
	  "X" => {"a ", TO "List", " or ", TO "Matrix"},
	  "A" => {"a ", TO "Ring", " or ", TO "Module"}
	  },
     Outputs => {ZZ},
     Caveat => {TT "regularSequenceCheck", " merely checks the injectivity of the maps in question. 
	  It does not check to see if ", TT "XA = A", "."},
     	  "Given a list ", TT "X", ", the function ", TT "regularSequenceCheck",
	  " gives an integer indicating how many initial elements of a ", TT "List", " form a regular sequence.",
     EXAMPLE lines ///
     A = ZZ[x_1..x_4]/(x_4^2)	  
     regularSequenceCheck({x_1..x_4},A)	    
     ///,
     PARA {
     	  "This symbol is provided by the package ", TO Depth, "."
     	  }
     }

-----------------------------------------------------------------------------

document {
     Key => {isRegularSequence,
	  (isRegularSequence,List,Ring),
  	  (isRegularSequence,Matrix,Module),
	  (isRegularSequence,List,Module),
	  (isRegularSequence,Matrix,Ring),
       	  (isRegularSequence,Matrix),
       	  (isRegularSequence,List)},
     Headline => "whether a list is regular over a ring or module",
     Usage => "isRegularSequence(X,A) or isRegularSequence(X) ",
     Inputs => {
	  "X" => {"a ", TO "List", " or ", TO "Matrix"},
	  "A" => {"a ", TO "Ring", " or ", TO "Module"}
	  },
     Outputs => {Boolean},
          "Given a list ", TT "X", ", the function ", TT "isRegularSequence", 
	  " tells if ", TT "X", " forms a regular sequence. If ", TT "X", 
	  " consists of homogeneous elements, it does this by comparing 
	  the hilbert series of ", TT "A", "
	  and the hilbert series of ", TT "A/XA", ". Otherwise it checks 
	  the injectivity of the maps defined by multiplication
	  by the elements of ", TT "X", " and also checks if ", TT "XA
	  = A", ".",
        EXAMPLE lines ///
	A = ZZ/2[x, y, z];
	X1 = {x, y*(x-1), z*(x-1)};
	isRegularSequence X1
	X2 = {z*(x-1), y*(x-1), x};
	isRegularSequence X2
	X3 = {1_A, x, y};
	isRegularSequence X3
	///,
      	PARA {
     	     "This symbol is provided by the package ", TO Depth, "."
     	     }
     	}
-----------------------------------------------------------------------------

document {
     Key => {regularSequence,
	  (regularSequence,Ideal,Ring),
	  (regularSequence,Ring),
	  Attempts,
	  Bound,
	  Sparseness,
	  Maximal,
	  [regularSequence,Attempts],
	  [regularSequence,Bound],
	  [regularSequence,Maximal],
	  [regularSequence,Sparseness]},
     Headline => "generates a regular sequence",
     Usage => "regularSequence(I,A)",
     Inputs => {
	  "I" => Ideal,
	  "A" => Ring,
	  Attempts => ZZ => "number of attempts made to generate a regular sequence",
	  Bound => ZZ => "bound on the value of the random coefficients",
	  Sparseness => RR => "between 0 and 1 giving the frequency of the coefficients being equal to zero",
	  Maximal => Boolean => "whether to insist on searching for a maximal regular sequence"
	  },
     Outputs => {Matrix},
     "Given a ring and an ideal, ", TT "regularSequence", " attempts
     to generate a regular sequence contained in ", TT "I", ". The
     algorithm is based on one found in Chapter 5.5 of W. Vasconcelos'
     book: ", EM "Computational Methods in Commutative Algebra and
     Algebraic Geometry", ".",
     EXAMPLE lines ///
     A = ZZ/5051[x, y, z];
     I = ideal (x, x*y, y*z);
     X = regularSequence(I,A)
     isRegularSequence(X,A)
     ///,
     "Here are examples with optional inputs:",
     EXAMPLE lines ///
     A = ZZ/5051[x, y, z];
     I = ideal (x, x*y, y*z);
     regularSequence(I,A,Attempts=>1,Bound=>100,Sparseness=>.9)
     ///,
     "Here are examples with the optional input ", TT "Maximal => false", ":",
     EXAMPLE lines ///
     x = symbol x; y = symbol y;
     n = 2;
     A = ZZ/101[x_(1,1)..x_(n,n),y_(1,1)..y_(n,n)];
     X = transpose genericMatrix(A,n,n);
     Y = transpose genericMatrix(A,y_(1,1),n,n);
     b = ideal(X*Y - Y*X);
     B = A/b;
     regularSequence(B,Attempts=>1,Maximal=>false)
     ///,
     PARA {
     	  "This symbol is provided by the package ", TO Depth, "."
     	  }
     }
-----------------------------------------------------------------------------

document {
     Key => {isCM,
	  (isCM,Module),
  	  (isCM,Ring)},
     Headline => "whether a ring or module is Cohen-Macaulay",
     Usage => "isCM(A)",
     Inputs => {
	  "A" => {"a ", TO "Ring", " or ", TO "Module"}
	  },
     Outputs => {TO "Boolean"},
     Caveat => {"Typically when one thinks of a Cohen-Macaulay ring or
module, one is in the local case. Since the local case is not yet
implemented into Macaulay 2, we compute over the ideal generated by by ", TO (gens, Ring), "."}, 
"This command merely checks if the depth of ", TT "A", " equals the Krull dimension of ", TT"A",".",
        EXAMPLE lines ///
	A = ZZ/2[x,y,z];
	isCM(A)
	A = ZZ/2[x,y]/(x^2,x*y);
	isCM(A)
	A =  ZZ/101[a_1,a_2,b_1,b_2,c_1]/ideal(a_1*b_1,a_2*b_2,b_1*c_1);
	isCM(A)
	///,
      PARA {
     "This symbol is provided by the package ", TO Depth, "."
     }
     }

--=========================================================================--
--=========================================================================--
--=========================================================================--
