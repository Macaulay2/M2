--=========================================================================--

newPackage(
     "Depth",
     Version => "0.3", 
     Date => "January 20, 2007",
     Authors => {
	  {Name => "Bart Snapp", Email => "snapp@math.uiuc.edu", HomePage => "http://www.math.uiuc.edu/~snapp/"}
	  },
     Headline => "aids in computations related to depth",
     DebuggingMode => true
     )

--=========================================================================--
     
export{regularSequence,isRegularSequence,isCM} 
-- if the new routines which you are adding have new
-- names, then they need to be exported; otherwise they should not be
-- exported
        
--=========================================================================--

-- regularElement  Give a regular element of an ideal.  Say in a polynoial ring how to find out if an element is a zerodivisor?
-- basiselementlimit, take gb (I,f-z^d) look at lead terms and see if z divides them. Homogenous
-- (I, fz-1)  elimate z 
-- write this for modules first.
-- keep in mind the substitute command...
-- keep in mind the eg (x*y,x*z,y*z) = I and finding a reg seq here.

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



------------------
depth(Ideal,Module) := ZZ => (I,M) -> (
     AI := (ring I)^1/I;
     for i from 0 to dim ring M do(
	  if Ext^i(AI,M) != 0 then return i); -- sees where the ext modules don't vanish
     infinity
     );

depth(Ideal,Ring) := ZZ => (I,A) -> (
     depth(I,A^1)
     );

depth(Ideal,PolynomialRing) := ZZ => (I,A) -> (
     codim I
     );



TEST /// 
A = QQ[x,y,z]/ideal(x^2)
m = ideal vars A
assert(depth(m,A) == 2)
depth(ideal(y),A)
A = ZZ[x,y]
m = ideal vars A
depth(m,A)
///


-----------------------------------------------------------------------------

regularSequence = method()
regularSequence(List, Module) := ZZ => (X,M) -> (
     X = splice X;
     for i from 0 to #X-1 do (
     	  f := X_i * id_M;
     	  if  not isInjective f  -- checks if map is injective
     	  then return i else M = coker f);
     #X);


TEST /// 
clearAll
A = ZZ/101[x_1..x_4]
regularSequence({x_1..x_4},A^1)
///

-----------------------------------------------------------------------------

regularSequence(List, Ring) := ZZ => (X,A) -> (
     regularSequence(X,A^1)
     );

-----------------------------------------------------------------------------
isRegularSequence = method()
isRegularSequence(List, Module) := Boolean => (X,M) -> (
     regularSequence(X,M) == #splice(X)
     );

-----------------------------------------------------------------------------

isRegularSequence(List, Ring) := Boolean => (X,A) -> (
     regularSequence(X,A) == #splice(X)
     );

-----------------------------------------------------------------------------

isCM = method()
isCM(Ideal,Ring) := Boolean => (I,A) -> (
     isCM(I,A^1)
     );

-----------------------------------------------------------------------------

isCM(Ideal,Module) := Boolean => (I,M) -> (
     dim(M) == depth(I,M)
     );

--=========================================================================--

beginDocumentation() -- the start of the documentation

-----------------------------------------------------------------------------

document { 
     Key => Depth,
     Headline => "aids in homological computations",
     EM "Depth", " is a package which will hopefully help users make homological computations."
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
     EXAMPLE {
	  "A = QQ[x_1..x_3]/ideal(x_1^2, x_1*x_2);",
	  "m = ideal vars A",
	  "depth(m,A)"
	  },
     "If ", TT "I", " contains a unit, then ", TT "depth(I,A)", " outputs ", TO "infinity", ".",
     EXAMPLE {
	  "depth(ideal(1),ZZ)"
	  },
     PARA {
     "This symbol is provided by the package ", TO Depth, "."
     }
     }

-----------------------------------------------------------------------------

document {
     Key => regularSequence,
     Headline => "how much of a list is regular",
     Usage => "regularSequence(X,A)",
     Inputs => {
	  "X" => {"a ", TO "List"},
	  "A" => {"a ", TO "Ring", " or ", TO "Module"}
	  },
     Outputs => {TO "ZZ"},
     Caveat => {TT "regularSequence", " merely checks the injectivity of the maps in question. It does not check to see if ", TT "XA = A", "."},
     "Given a list ", TT "X", ", the function ", TT "regularSequence",
" gives an integer indicating how many initial elements of a ", TT "List", " form a regular sequence.",
     EXAMPLE {
	  "A = ZZ[x_1..x_4]/(x_4^2)",
	  "regularSequence({x_1..x_4},A)"
	  },
     PARA {
     "This symbol is provided by the package ", TO Depth, "."
     }
     }

-----------------------------------------------------------------------------

document {
     Key => isRegularSequence,
     Headline => "whether a list is regular over a ring or module",
     Usage => "isRegularSequence(X,A)",
     Inputs => {
	  "X" => {"a ", TO "List"},
	  "A" => {"a ", TO "Ring", " or ", TO "Module"}
	  },
     Outputs => {TO "Boolean"},
     Caveat => {TT "regularSequence", " merely checks the injectivity of the maps in question. It does not check to see if ", TT "XA = A", "."},
          "Given a list ", TT "X", ", the function ", TT "isRegularSequence",
" tells if ", TT "X", " forms a regular sequence.",
        EXAMPLE {
	  "A = ZZ/2[x, y, z];",
	  "X1 = {x, y*(x-1), z*(x-1)};",
	  "isRegularSequence(X1, A)",
	  "X2 = {z*(x-1), y*(x-1), x};",
	  "isRegularSequence(X2, A)",
	  "X3 = {1, x, y};",
	  "isRegularSequence(X3, A)"
	  },
      PARA {
     "This symbol is provided by the package ", TO Depth, "."
     }
     }

-----------------------------------------------------------------------------

document {
     Key => isCM,
     Headline => "whether a ring or module is Cohen-Macaulay",
     Usage => "isCM(I,A)",
     Inputs => {
	  "I" => {"an ", TO "Ideal"},
	  "A" => {"a ", TO "Ring", " or ", TO "Module"}
	  },
     Outputs => {TO "Boolean"},
     Caveat => {"Typically when one thinks of a Cohen-Macaulay ring or
module, one is in the local case. Since the local case is not yet
implemented into Macaulay 2, we insist that the user give an ideal for
computing the depth."}, 
"This command merely checks if the ", TT "I","-depth of ", TT "A", " equals the Krull dimension of ", TT"A",".",
        EXAMPLE {
	  "A = ZZ/2[x,y,z];",
	  "m = ideal(x,y,z);",
	  "isCM(m,A)",
      	  "A = ZZ/2[x,y]/(x^2,x*y);",
	  "m = ideal(x,y);",
	  "isCM(m,A)",
	  "A =  ZZ/101[a_1,a_2,b_1,b_2,c_1]/ideal(a_1*b_1,a_2*b_2,b_1*c_1);",
	  "m = ideal vars A",
	  "isCM(m,A)"
	  },
      PARA {
     "This symbol is provided by the package ", TO Depth, "."
     }
     }

--=========================================================================--
