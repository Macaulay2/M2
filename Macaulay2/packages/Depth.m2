--=========================================================================--
--=========================================================================--
--=========================================================================--

newPackage(
     "Depth",
     Version => "0.4.2", 
     Date => "March 21, 2008",
     Authors => {
	  {Name => "Bart Snapp", Email => "snapp@coastal.edu", HomePage => "http://ww2.coastal.edu/snapp/"}
	  },
     Headline => "aids in computations related to depth",
     DebuggingMode => true
     )

--=========================================================================--
     
export{regularSequenceCheck,isRegularSequence,regularSequence,isCM,Sparseness,Bound,Attempts,Maximal} 
        
--=========================================================================--

-- All this does is check where the ext modules don't vanish.

depth(Ideal,Module) := ZZ => (I,M) -> (
     AI := (ring I)^1/I;
     for i from 0 to dim ring M do(
	  if Ext^i(AI,M) != 0 then return i); 
     infinity
     )

-----------------------------------------------------------------------------

depth(Ideal,Ring) := ZZ => (I,A) -> (
     depth(I,module A)
     )

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
clearAll
A = ZZ/101[x_1..x_4]
regularSequenceCheck({x_1..x_4},A^1)
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
     regularSequenceCheck(X,M) == #splice(X) and ideal(X)*M != M
     )

-----------------------------------------------------------------------------

isRegularSequence(Matrix, Module) := Boolean => (X,M) -> (
     regularSequenceCheck(X,M) == #splice(flatten entries X) and ideal(X)*M != M
     )

-----------------------------------------------------------------------------

isRegularSequence(List, Ring) := Boolean => (X,A) -> (
     regularSequenceCheck(X,A) == #splice(X) and ideal(X) != 1
     )

-----------------------------------------------------------------------------

isRegularSequence(Matrix, Ring) := Boolean => (X,A) -> (
     regularSequenceCheck(X,A) == #splice(flatten entries X) and ideal(X) != 1
     )

--=========================================================================--

regularSequence = method(Options => {Sparseness => .5, Bound => 1, Attempts => 100, Maximal => true})
regularSequence(Ideal,Ring) := Matrix => opts -> (I,A) -> (
     k := coefficientRing A;
     f := gens I;
     r := numgens source f;
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
assert(regularSequence(I,A,Attempts=>1,Bound=>100,Sparseness=>.9) - matrix{{82*x, 95*y*z}}==0)
///
	       
--=========================================================================--

isCM = method()
isCM(Ideal,Ring) := Boolean => (I,A) -> (
     isCM(I,A^1)
     )

-----------------------------------------------------------------------------

isCM(Ideal,Module) := Boolean => (I,M) -> (
     dim(M) == depth(I,M)
     )

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
     Key => (depth, Ideal, Ring),
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
"-depth of a ring. It does this by computing ", TT "Ext^i(A^1/I,A)", 
" and noting where it does not vanish.",
     EXAMPLE lines ///
     A = QQ[x_1..x_3]/ideal(x_1^2, x_1*x_2);
     m = ideal vars A
     depth(m,A)
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
  	  (isRegularSequence,Matrix,Ring)},
     Headline => "whether a list is regular over a ring or module",
     Usage => "isRegularSequence(X,A)",
     Inputs => {
	  "X" => {"a ", TO "List", " or ", TO "Matrix"},
	  "A" => {"a ", TO "Ring", " or ", TO "Module"}
	  },
     Outputs => {Boolean},
          "Given a list ", TT "X", ", the function ", TT "isRegularSequence", " tells if ", 
	  TT "X", " forms a regular sequence. It does this by checking the injectivity of the maps defined 
	  by multiplication by the elements of ", TT "X", " and checks if ", TT "XA = A", ".",
          EXAMPLE lines ///
	A = ZZ/2[x, y, z];
	X1 = {x, y*(x-1), z*(x-1)};
	isRegularSequence(X1, A)
	X2 = {z*(x-1), y*(x-1), x};
	isRegularSequence(X2, A)
	X3 = {1, x, y};
	isRegularSequence(X3, A)
	///,
      	PARA {
     	     "This symbol is provided by the package ", TO Depth, "."
     	     }
     	}
-----------------------------------------------------------------------------

document {
     Key => {regularSequence,
	  (regularSequence,Ideal,Ring),
	  Attempts,
	  Bound,
	  Sparseness,
	  Maximal,
	  [regularSequence,Attempts],
	  [regularSequence,Bound],
	  [regularSequence,Maximal],
	  [regularSequence,Sparseness]
	  },
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
     "Given a ring and an ideal, ", TT "regularSequence", " attempts to generate a regular sequence contained in ", TT "I", ".", 
     "The algorithm is based on one found in Chapter 5.5 of W. Vasconcelos' book: ", EM "Computational Methods in Commutative Algebra and Algebraic Geometry", ".",
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
     bracket = ideal flatten (X*Y - Y*X);
     B = A/bracket;
     m = ideal gens B;
     regularSequence(m,B,Attempts=>1,Maximal=>false)
     ///,
     "Note, considering the example above, we can compute a regular sequence ", EM "faster", " than we can compute the depth:",
     EXAMPLE lines ///
     n = 2;
     A = ZZ/101[x_(1,1)..x_(n,n),y_(1,1)..y_(n,n)];
     X = transpose genericMatrix(A,n,n);
     Y = transpose genericMatrix(A,y_(1,1),n,n);
     bracket = ideal flatten (X*Y - Y*X);
     B = A/bracket;
     m = ideal gens B;
     time regularSequence(m,B)
     time depth(m,B)
     ///,
     PARA {
     	  "This symbol is provided by the package ", TO Depth, "."
     	  }
     }
-----------------------------------------------------------------------------

document {
     Key => {isCM,
	  (isCM,Ideal,Module),
  	  (isCM,Ideal,Ring)},
     Headline => "whether a ring or module is Cohen-Macaulay",
     Usage => "isCM(I,A)",
     Inputs => {
	  "I" => Ideal,
	  "A" => {"a ", TO "Ring", " or ", TO "Module"}
	  },
     Outputs => {TO "Boolean"},
     Caveat => {"Typically when one thinks of a Cohen-Macaulay ring or
module, one is in the local case. Since the local case is not yet
implemented into Macaulay 2, we insist that the user give an ideal for
computing the depth."}, 
"This command merely checks if the ", TT "I","-depth of ", TT "A", " equals the Krull dimension of ", TT"A",".",
        EXAMPLE lines ///
	A = ZZ/2[x,y,z];
	m = ideal(x,y,z);
	isCM(m,A)
	A = ZZ/2[x,y]/(x^2,x*y);
	m = ideal(x,y);
	isCM(m,A)
	A =  ZZ/101[a_1,a_2,b_1,b_2,c_1]/ideal(a_1*b_1,a_2*b_2,b_1*c_1);
	m = ideal vars A
	isCM(m,A)
	///,
      PARA {
     "This symbol is provided by the package ", TO Depth, "."
     }
     }

--=========================================================================--
--=========================================================================--
--=========================================================================--



-- regularElement Gives a regular element of an ideal.  Say in a
-- polynoial ring how to find out if an element is a zerodivisor?
-- basiselementlimit, take gb (I,f-z^d) look at lead terms and see if
-- z divides them. Homogeneous (I, fz-1) eliminate z write this for
-- modules first.  keep in mind the substitute command...  keep in
-- mind the eg (x*y,x*z,y*z) = I and finding a reg seq here.

--A = ZZ[x,y,z]
--M = A^1/ideal(y*z)
--I = ideal(x*y,x*z,y*z)
--g = gens gb I
--g_(0,2)
--isInjective((g_(0,1))*id_M)

--regularElement(Ideal,Module) := RingElement => (I,M) -> (
--     g gens gb I
--     isInjective g_0 *id_M
--    );




