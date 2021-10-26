undocumented {(net,NCGroebnerBasis),
              (net,NCIdeal),
	      (net,NCLeftIdeal),
	      (net,NCRightIdeal),
	      (net,NCRing),
	      (net,NCRingElement),
	      (net,NCMatrix),
	      (net,NCRingMap),
	      (net,NCChainComplex),
	      (expression, NCMatrix),
	      (net,NCQuotientRing),
	      functionHash,
	      (NewFromMethod,NCPolynomialRing,List),
	      (NewFromMethod,NCQuotientRing,List),
	      (symbol ?, NCMatrix, NCMatrix),
	      (symbol ?, NCRingMap, NCRingMap),
	      CacheBergmanGB,
	      MakeMonic,
	      Derivation,
	      NumModuleVars,
	      InstallGB,
	      ReturnIdeal,
	      NumberOfBins,
	      Basis}

beginDocumentation()

-------------------------
----- Types
-------------------------
    
doc ///
  Key
    NCAlgebra
  Description
    Text
      This package is used to define and manipulate noncommutative algebras.  Many of the
      commands contain calls to the existing noncommutative algebra package Bergman.
      
      Detailed instructions for installing Bergman, as well as the NCAlgebra system, can be
      found in the file installNCAlgebra.txt file contained in the NCAlgebra package directory.
      It may also be found at @HREF"https://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/NCAlgebra/installNCAlgebra.txt"@.
	 
  Subnodes
    "Basic operations on noncommutative algebras"
    "General setup information"
    "Using the Bergman interface"
///

doc ///
   Key
      NCRing
   Headline
      Type of a noncommutative ring
   Description
      Text
         All noncommutative rings have this as an ancestor type.  It is the parent of the
	 types @ TO NCPolynomialRing @ and @ TO NCQuotientRing @. 
      Text
         In addition to defining a ring as a quotient of a @ TO NCPolynomialRing @, some common ways to create
	 NCRings include @ TO skewPolynomialRing @, @ TO endomorphismRing @, and @ TO oreExtension @.      
      
         Let's consider a three dimensional Sklyanin algebra.  We first define the tensor algebra:
      Example
         A = QQ{x,y,z}
      Text
         Then input the defining relations, and put them in an ideal:
      Example
	 f = y*z + z*y - x^2
	 g = x*z + z*x - y^2
	 h = z^2 - x*y - y*x
     	 I=ncIdeal{f,g,h}
      Text
         Next, define the quotient ring (as well as try a few functions on this new ring).  Note that
	 when the quotient ring is defined, a call is made to Bergman to compute the Groebner basis
	 of I (out to a certain degree, should the Groebner basis be infinite).
      Example
	 B=A/I
	 generators B
	 numgens B
	 isCommutative B
	 coefficientRing B
      Text
	 As we can see, x is an element of B.
      Example
         x
      Text
         If we define a new ring containing x, x is now part of that new ring
      Example
      	 C = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w}) 
         x
      Text
         We can 'go back' to B using the command @ TO (use, NCRing) @.
      Example
	 use B
	 x
	 use C
      Text
         We can also create an Ore extension.  First define a @ TO NCRingMap @ with @ TO ncMap @.
      Example
	 sigma = ncMap(C,C,{y,z,w,x})
      Text
         Then call the command @ TO oreExtension @.
      Example
	 D = oreExtension(C,sigma,a)
	 generators D
	 numgens D
   SeeAlso
      "Basic operations on noncommutative algebras"
///

doc ///
  Key
    (generators, NCRing)
  Headline
    The list of algebra generators of an NCRing
  Usage
    gensA = generators A
  Inputs
    A : NCRing
  Outputs
    gensA : List
  Description
    Text
       This function returns the generators of an NCRing as a list.  As usual,
       gens is a synonym for generators.
    Example
       A = QQ{x,y,z}
       generators A
       gens A
///

doc ///
  Key
    (numgens, NCRing)
  Headline
    The number of algebra generators of an NCRing
  Usage
    numgensA = numgens A
  Inputs
    A : NCRing
  Outputs
    numgensA : ZZ
  Description
    Text
       This function returns the number of generators of an NCRing.
    Example
       A = QQ{x,y,z}
       numgens A
///

doc ///
  Key
    (isCommutative, NCRing)
    isExterior
    (isExterior, NCRing)
    (isExterior, Ring)
  Headline
    Returns whether an NCRing is commutative
  Usage
    isCommutative A or isExterior A
  Inputs
    A : NCRing
  Outputs
    : Boolean
  Description
    Text
       This function returns whether an NCRing is commutative or a quotient
       of the exterior algebra
    Example
       A = QQ{x,y,z}
       isCommutative A
       B = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z})
       isCommutative B
       C = skewPolynomialRing(QQ,1_QQ,{x,y,z})
       isCommutative C
       D = toNCRing(QQ[x,y,SkewCommutative=>true])
       isExterior D
///

doc ///
  Key
    (coefficientRing, NCRing)
  Headline
    Returns the base ring of an NCRing
  Usage
    k = coefficientRing NCRing
  Inputs
    A : NCRing
  Outputs
    k : Ring
  Description
    Text
       This function returns the base ring of an NCRing
    Example
       A = QQ{x,y,z}
       coefficientRing A
       R = ZZ/101[a,b,c,d]/(ideal(a^2-b^2))
       B = R{x,y,z}
       coefficientRing B
///

doc ///
  Key
     (use, NCRing)
  Headline
     Brings the variables of a particular NCRing in scope
  Usage
    use A
  Inputs
    A : NCRing
  Description
    Text
       This function brings the variables of a particular NCRing in scope.
       For an illustration:
    Example
       A = QQ{x,y,z}
       coefficientRing A
       B = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z})
       x
    Text
       As you can see, at this point the interpreter treats x,y and z as elements of B.  To go back to
       A, we run the command use A:
    Example
       use A
       x
///

doc ///
   Key
      NCPolynomialRing
   Headline
      Type of a noncommutative polynomial ring
   Usage
      A = QQ{x,y}
   Description
      Text
         This is the type of a noncommutative polynomial ring over a commutative
	 ring R (i.e. a tensor algebra over R).  It has parent type @ TO NCRing @.
      Example
         A = QQ{x,y}
///

doc ///
   Key
      (ideal, NCPolynomialRing)
   Headline
      The defining ideal of an NCPolynomialRing
   Usage
      I = ideal A
   Inputs
      A : NCPolynomialRing
   Outputs
      I : NCIdeal
   Description
      Text
         This returns the defining ideal of an NCPolynomialRing, which 
	 will be the zero ideal in the noncommutative polynomial ring.
      Example
         A = QQ{x,y}
	 ideal A
///

doc ///
   Key
      NCQuotientRing
   Headline
      Type of a noncommutative ring
   Description
      Text
         This is the type of a quotient of a tensor algebra by a two-sided ideal.
    
         At this point, one cannot define quotients of quotients.
///

doc ///
   Key
     (symbol /, NCPolynomialRing, NCIdeal)
   Headline
     Construct a NCQuotientRing
   Usage
     B = A/I
   Inputs
     A : NCPolynomialRing
     I : NCIdeal
   Outputs
     B : NCQuotientRing
   Description
      Text
         This is one way to create a quotient of the tensor algebra modulo some relations.
    
         At this point, one cannot define quotients of quotients.
	 
	 If the base ring is QQ or a finite field of order p, then Bergman is called to compute a
	 Groebner basis.
	 
	 If not, then one has a couple of options.  The first is to take the defining ideal of the algebra, and provide a
	 Groebner Basis by calling @ TO ncGroebnerBasis @ with the InstallGB flag set to true.  Of course, if this generating
	 set is not a Groebner basis, then you will get incorrect results upon calls to functions like @ TO (basis, ZZ, NCRing) @.
	 
	 The alternative is to use the built in commands @ TO skewPolynomialRing @ and @ TO oreExtension @ which
	 has the same effect as above occurring behind the scenes.  Just be careful using these commands to create your
	 ring if your base ring is not a field Bergman can work with, as the generating sets created may not be a Groebner
	 basis for the defining ideal (this is more often a problem for @ TO oreExtension @ than @ TO skewPolynomialRing @).
      Example
         A = QQ{x,y,z}
         f = y*z + z*y - x^2
	 g = x*z + z*x - y^2
	 h = z^2 - x*y - y*x
     	 I=ncIdeal{f,g,h}
    	 B = A/I
         z^2
	 R = toField(QQ[a]/ideal(a^4+a^3+a^2+a+1))
	 C = skewPolynomialRing(R,a,{x,y,z})
	 y*x
///

doc ///
   Key
     (ambient, NCQuotientRing)
   Headline
     Ambient ring of an NCQuotientRing
   Usage
     A = ambient B 
   Inputs
     B : NCQuotientRing
   Outputs
     A : NCPolynomialRing
   Description
      Text
         Returns the ambient ring of an @ TO NCQuotientRing @.  
	 
	 As quotients of NCQuotientRings are added, this will return the top-level ambient ring.
	 
      Example
         B = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z})
	 A = ambient B
///

doc ///
   Key
     (ideal, NCQuotientRing)
   Headline
     Defining ideal of an NCQuotientRing in its ambient ring
   Usage
     I = ideal B
   Inputs
     B : NCQuotientRing
   Outputs
     I : NCIdeal
   Description
      Text
         This returns the defining ideal of an NCQuotientRing in its ambient ring.  As of now,
	 this is always an ideal in an NCPolynomialRing, but when quotients of @ TO NCQuotientRing @s
	 are added, this will no longer be the case.
      Example
         B = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z})
	 A = ambient B
	 I = ideal B
	 ring I === A
///

doc ///
   Key
      NCMatrix
   Headline
      Type of a matrix over a noncommutative ring
   Description
      Text
         This is the type of a matrix over a noncommutative ring.  These represent homomorphisms between two free modules in
	 chosen bases (whether you think of it as a map of left or right modules is up you).  Modules themselves are not
	 implemented yet in the @ TO NCAlgebra @ package, but are slated for a later release.
      Text
         Common ways to make (and use) a matrix include
      Code
         UL {TO (ncMatrix, List),
	     TO (basis, ZZ, NCRing),
	     TO (rightKernel, NCMatrix, ZZ),
	     TO (rightKernelBergman, NCMatrix)}
      Text
         Common ways to get information about matrices
      Code
         UL {TO (ring, NCMatrix),
	     TO (entries, NCMatrix)}
      Text
         Common operations on matrices:
      Code
         UL {TO (symbol +, NCMatrix,NCMatrix),
	     TO (symbol -, NCMatrix,NCMatrix),
	     TO (symbol %, NCMatrix,NCGroebnerBasis),
             TO (symbol *, NCMatrix,NCMatrix),
	     TO (symbol *, NCMatrix,NCRingElement),
	     TO (symbol *, NCMatrix,RingElement),
	     TO (symbol //, NCMatrix,NCMatrix),
	     TO (symbol _, NCMatrix,List),
	     TO (symbol ==, NCMatrix, NCMatrix),
	     TO (symbol |, NCMatrix,NCMatrix),
	     TO (symbol ||, NCMatrix,NCMatrix),
	     TO (symbol ^, NCMatrix,List),
	     TO (symbol ^, NCMatrix,ZZ),
	     }
      Text
         This is the type of a matrix with entries in an NCRing.  Many of the basic operations
	 one can perform on a @ TO Matrix @ are also allowed with an @ TO NCMatrix @, and
	 the behavior of the functions should be similar to the corresponding 'usual' command.
	 Some examples of creating and using NCMatrices are given below.
      Example
         A = QQ{a,b,c,d}
	 M = ncMatrix {{a,b,c,d}}
	 N = ncMatrix {{M,2*M,3*M},{4*M,5*M,6*M}}

         B = QQ{x,y,z}
	 f = y*z + z*y - x^2
	 g = x*z + z*x - y^2
	 h = z^2 - x*y - y*x
	 I = ncIdeal {f,g,h}
	 Igb = ncGroebnerBasis I
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(B,B,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 Nred = N^3 % Igb
	 C = B/I
	 phi = ncMap(C,B,gens C)
	 NC = phi N
	 N3C = NC^3
	 X = NC + 3*NC
	 Y = NC | 2*NC
	 Z = X || NC
///

doc ///
   Key
      ncMatrix
      (ncMatrix,List)
      (ncMatrix,NCRing,List,List)
   Headline
      Create an NCMatrix
   Usage
      M = ncMatrix entriesList
   Inputs
      entriesList : List
   Outputs
      M : NCMatrix
   Description
      Text
         This command creates an NCMatrix.  As with the @ TO matrix @ command, the user
	 may provide this matrix as a doubly nested list of NCRingElements, or as a
	 doubly nested list of NCMatrices.
      
        The ncMatrix(NCRing,List,List) constructor is used only when creating maps to and from
	the zero free module.
      Example
         A = QQ{a,b,c,d}
	 M = ncMatrix {{a,b,c,d}}
	 N = ncMatrix {{M,2*M,3*M},{4*M,5*M,6*M}}
///

doc ///
   Key
      (symbol -, NCMatrix, NCMatrix)
   Headline
      Subtract NCMatrices
   Usage
      L = M - N
   Inputs
     M : NCMatrix
     N : NCMatrix
   Outputs
     L : NCMatrix
   Description
      Text
         This subtracts NCMatrices.
      Example
         A = QQ{x,y,z}
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(A,A,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 N' = ncMatrix {{sigma sigma M}, {sigma M}, {M}}
	 N - N'
///

doc ///
   Key
      (symbol -, NCMatrix)
   Headline
      Negates NCMatrices
   Usage
     L = -M
   Inputs
     M : NCMatrix
   Outputs
     L : NCMatrix
   Description
      Text
         This negates NCMatrices.
      Example
         A = QQ{x,y,z}
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(A,A,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 -N
///

doc ///
   Key
      (symbol +, NCMatrix, NCMatrix)
   Headline
      Add NCMatrices
   Usage
      L = M + N
   Inputs
     M : NCMatrix
     N : NCMatrix
   Outputs
     L : NCMatrix
   Description
      Text
         This adds NCMatrices.
      Example
         A = QQ{x,y,z}
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(A,A,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 N' = ncMatrix {{sigma sigma M}, {sigma M}, {M}}
	 N + N'
///

doc ///
   Key
      (symbol %, NCMatrix, NCGroebnerBasis)
   Headline
      Reduces the entries of an NCMatrix with respect to an NCGroebnerBasis
   Usage
      L = M % Igb
   Inputs
     M : NCMatrix
     Igb : NCGroebnerBasis
   Outputs
     L : NCMatrix
   Description
      Text
         This command reduces the entries of an NCMatrix with respect to an NCGroebnerBasis.
      Example
         A = QQ{x,y,z}
	 f = y*z + z*y - x^2
	 g = x*z + z*x - y^2
	 h = z^2 - x*y - y*x
	 I = ncIdeal {f,g,h}
	 Igb = ncGroebnerBasis I
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(A,A,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 N3 = N^3
	 N3red = N3 % Igb
///

doc ///
   Key
      (symbol *, NCMatrix, NCMatrix)
   Headline
      Product of NCMatrices
   Usage
      L = M*N
   Inputs
     M : NCMatrix
     N : NCMatrix
   Outputs
     L : NCMatrix
   Description
      Text
         This command allows for the product of composable NCMatrices (or ordinary matrices over the base).
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(B,B,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 N' = ncMatrix {{sigma sigma M}, {sigma M}, {M}}
	 N*N'
	 N'*N
///

doc ///
   Key
      (symbol *, NCMatrix, Matrix)
   Headline
      Product of NCMatrices
   Usage
      L = M*N
   Inputs
     M : NCMatrix
     N : Matrix
   Outputs
     L : NCMatrix
   Description
      Text
         This command allows for the product of composable NCMatrices (or ordinary matrices over the base).
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(B,B,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
         L = map(QQ^3,QQ^3,{{2,0,0},{1,2,0},{1,2,3}})
	 N*L
///

doc ///
   Key
      (symbol *, NCMatrix, NCRingElement)
   Headline
      Product of NCMatrices
   Usage
      L = M*f
   Inputs
     M : NCMatrix
     f : NCRingElement
   Outputs
     L : NCMatrix
   Description
      Text
         This command allows for the scalar multiplication of an NCMatrix by an NCRingElement on the right.
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(B,B,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 N*x^2
///

doc ///
   Key
      (symbol *, NCMatrix, RingElement)
   Headline
      Product of NCMatrices
   Usage
      L = M*f
   Inputs
     M : NCMatrix
     N : RingElement
   Outputs
     L : NCMatrix
   Description
      Text
         This command allows for the scaling of an NCMatrix by an element in the base ring.
      Example
         R = frac(QQ[a])
	 B = skewPolynomialRing(R,a,{x,y,z})
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(B,B,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
    	 N*a
///

doc ///
   Key
      (symbol *, NCMatrix, QQ)
   Headline
      Product of NCMatrices
   Usage
      L = M*a
   Inputs
     M : NCMatrix
     a : QQ
   Outputs
     L : NCMatrix
   Description
      Text
         This command allows for the scaling of an @ TO NCMatrix @ by an element in @ TO QQ @.
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(B,B,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 N*(1/2)
///

doc ///
   Key
      (symbol *, NCMatrix, ZZ)
   Headline
      Product of NCMatrices
   Usage
      L = M*a
   Inputs
     M : NCMatrix
     a : ZZ
   Outputs
     L : NCMatrix
   Description
      Text
         This command allows for the scaling of an @ TO NCMatrix @ by an element in @ TO ZZ @.
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(B,B,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 N*3
///

doc ///
   Key
      (symbol *, Matrix, NCMatrix)
   Headline
      Product of NCMatrices
   Usage
      L = N*M
   Inputs
     N : Matrix
     M : NCMatrix
   Outputs
     L : NCMatrix
   Description
      Text
         This command allows for the product of composable NCMatrices (or ordinary matrices over the base).
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(B,B,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
         L = map(QQ^3,QQ^3,{{2,0,0},{1,2,0},{1,2,3}})
	 L*N
///

doc ///
   Key
      (symbol *, NCRingElement, NCMatrix)
   Headline
      Product of NCMatrices
   Usage
      L = f*M
   Inputs
     f : NCRingElement
     M : NCMatrix
   Outputs
     L : NCMatrix
   Description
      Text
         This command allows for the scalar multiplication of an NCMatrix by an NCRingElement on the left.
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(B,B,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 x^2*N
///

doc ///
   Key
      (symbol *, RingElement, NCMatrix)
   Headline
      Product of NCMatrices
   Usage
      L = f*M
   Inputs
     f : RingElement
     M : NCMatrix
   Outputs
     L : NCMatrix
   Description
      Text
         This command allows for the scaling of an NCMatrix by an element in the base ring.
      Example
         R = frac(QQ[a])
	 B = skewPolynomialRing(R,a,{x,y,z})
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(B,B,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
    	 a*N
///

doc ///
   Key
      (symbol *, QQ, NCMatrix)
   Headline
      Product of NCMatrices
   Usage
      L = a*M
   Inputs
     a : QQ
     M : NCMatrix
   Outputs
     L : NCMatrix
   Description
      Text
         This command allows for the scaling of an @ TO NCMatrix @ by an element in @ TO QQ @.
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(B,B,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 (1/2)*N
///

doc ///
   Key
      (symbol *, ZZ, NCMatrix)
   Headline
      Product of NCMatrices
   Usage
      L = a*M
   Inputs
     a : ZZ
     M : NCMatrix
   Outputs
     L : NCMatrix
   Description
      Text
         This command allows for the scaling of an @ TO NCMatrix @ by an element in @ TO ZZ @.
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(B,B,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 3*N
///

doc ///
   Key
      (symbol ==, NCMatrix, NCMatrix)
      (symbol ==, NCMatrix, ZZ)
      (symbol ==, ZZ, NCMatrix)
   Headline
      Test equality of matrices
   Usage
      isEqual = M == N
   Inputs
      M : NCMatrix
          or an integer
      N : NCMatrix
          or an integer
   Outputs
      isEqual : Boolean
   Description
      Text
         This command tests equality for matrices.  If one of the inputs is an integer, then the test
	 only will work if the integer is zero.  Below, we test the well-definedness of the exponentiation
	 operation using Groebner bases.
      Example
         A = QQ{x,y,z}
	 f = y*z + z*y - x^2
	 g = x*z + z*x - y^2
	 h = z^2 - x*y - y*x
	 I = ncIdeal {f,g,h}
	 Igb = ncGroebnerBasis I
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(A,A,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 Nred = N^3 % Igb
	 B = A/I
	 phi = ncMap(B,A,gens B)
	 NB = phi N
	 N3B = NB^3
	 (phi Nred) == N3B
///

doc ///
   Key
      (symbol |, NCMatrix, NCMatrix)
   Headline
      Join NCMatrices horizontally
   Usage
      L = M | N
   Inputs
      M : NCMatrix
      N : NCMatrix
   Outputs
      L : NCMatrix
   Description
      Text
         This command joins NCMatrices horizontally.
      Example
         A = QQ{a,b,c,d}
	 M = ncMatrix {{a,b,c,d},{b,c,d,a}}
	 N = M | 2*M | -3*M
///

doc ///
   Key
      (symbol ||, NCMatrix, NCMatrix)
   Headline
      Join NCMatrices vertically
   Usage
      L = M || N
   Inputs
      M : NCMatrix
      N : NCMatrix
   Outputs
      L : NCMatrix
   Description
      Text
         This command joins NCMatrices vertically.
      Example
         A = QQ{a,b,c,d}
	 M = ncMatrix {{a,b,c,d},{b,c,d,a}}
	 N = M || 2*M || -3*M
///

doc ///
   Key
      (symbol ^, NCMatrix, List)
   Headline
      Select some rows of an NCMatrix
   Usage
      L = M^rows
   Inputs
      M : NCMatrix
      rows : List
   Outputs
      L : NCMatrix
   Description
      Text
         This command selects some rows of an NCMatrix.
      Example
         A = QQ{a,b,c,d}
	 M = ncMatrix {{a,b,c,d},{b,c,d,a}}
	 N = M || 2*M || -3*M
	 N^{0,3,4}
///

doc ///
   Key
      (symbol _, NCMatrix, List)
   Headline
      Select some columns of an NCMatrix
   Usage
      L = M_cols
   Inputs
      M : NCMatrix
      cols : List
   Outputs
      L : NCMatrix
   Description
      Text
         This command selects some columns of an NCMatrix.
      Example
         A = QQ{a,b,c,d}
	 M = ncMatrix {{a,b,c,d},{b,c,d,a}}
	 N = M || 2*M || -3*M
	 N_{0,2}
///

doc ///
   Key
      (symbol ^, NCMatrix, ZZ)
   Headline
      Exponentiate an NCMatrix
   Usage
      L = M^n
   Inputs
      M : NCMatrix
      n : ZZ
   Outputs
      L : NCMatrix
   Description
      Text
         This exponentiates an NCMatrix.  It should be remarked that the matrix is reduced
	 with the GB of the ring it is over on each iteration of the product.  If your algebra
	 is significantly smaller than the tensor algebra, this is a large savings.
	 The input is assumed to be a nonnegative integer at this time.
      Example
         A = QQ{x,y,z}
	 M = ncMatrix {{x, y, z}}
	 sigma = ncMap(A,A,{y,z,x})
	 N = ncMatrix {{M},{sigma M}, {sigma sigma M}}
	 N^3
	 B = A/ncIdeal{y*z + z*y - x^2, x*z + z*x - y^2, z^2 - x*y - y*x}
	 NB = promote(N,B)
	 NB^3
///

doc ///
   Key
      (symbol //, NCMatrix, NCMatrix)
   Headline
      Factor one map through another
   Usage
      L = M // N
   Inputs
      M : NCMatrix
      N : NCMatrix
   Outputs
      L : NCMatrix
   Description
      Text
         This command factors one map through another.  One nice application
	 of this is to compute twisted matrix factorizations.  
	 If the maps input are homogeneous, then the degrees must match up for the command to work.

         If M does not factor through N, then the return value L is such that M - N*L is the reduction
	 of M modulo a Groebner basis for the image of N.

	 Here is an example of doing so over a PI Sklyanin algebra.  Note that since quotients of
	 quotients are (unfortunately) not yet implemented, we have to do a bit of acrobatics
	 to define the quotient of the Sklyanin we want.
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
         A = ambient B
      Text
         The element g below is central in B (the two is just for convenience).
      Example
	 g = 2*(-y^3-x*y*z+y*x*z+x^3)
	 J = (ideal B) + ncIdeal {g}
	 B' = A/J -- Factor of sklyanin
	 k = ncMatrix {{x,y,z}}
	 BprimeToB = ncMap(B,B',gens B) -- way to lift back from B' to B
	 M = BprimeToB rightKernelBergman rightKernelBergman k  -- second syzygy of k over B
      Text
         At this point, M is maximal Cohen-Macaulay B'-module,
	 and hence the projective dimension of M as a B-module
	 is 1.  Since M is a B' module, multiplication by g on the
	 complex that gives the resolution over B is null homotopic.  This means
	 we may factor the map f through f times the identity.  We do so below.
      Example
	 gId = g*(ncMatrix applyTable(entries id_(ZZ^4), i -> promote(i,B)))
	 assignDegrees(gId,{2,2,2,3},{5,5,5,6});
	 -- now factor through g*id
	 M' = gId // M
	 M*M' == gId
///

doc ///
   Key
      (transpose, NCMatrix)
   Headline
      Transposes an NCMatrix
   Usage
      L = transpose M
   Inputs
      M : NCMatrix
   Outputs
      L : NCMatrix
   Description
      Text
         This command transposes an NCMatrix
      Example
         A = QQ{a,b,c,d}
	 M = ncMatrix {{a,b,c,d},{b,c,d,a}}
	 N = M || 2*M || -3*M
	 transpose N
///

doc ///
   Key
      (ring, NCMatrix)
   Headline
      Gives the ring of the NCMatrix
   Usage
      L = ring M
   Inputs
      M : NCMatrix
   Outputs
      L : NCRing
   Description
      Text
         This command returns the ring over which the NCMatrix is defined.
      Example
         A = QQ{a,b,c,d}
	 M = ncMatrix {{a,b,c,d},{b,c,d,a}}
    	 ring M
///

doc ///
   Key
      (entries, NCMatrix)
   Headline
      Returns the entries of the NCMatrix
   Usage
      L = entries M
   Inputs
      M : NCMatrix
   Outputs
      L : List
   Description
      Text
         Returns the entries of the NCMatrix as a doubly nested list.
      Example
         A = QQ{a,b,c,d}
	 M = ncMatrix {{a,b,c,d},{b,c,d,a}}
	 N = M || 2*M || -3*M
	 entries N
///

doc ///
   Key
      (lift, NCMatrix)
   Headline
      Lifts an NCMatrix
   Usage
      L = lift M
   Inputs
      M : NCMatrix
   Outputs
      L : NCMatrix
   Description
      Text
         This command lifts an NCMatrix to a matrix over its @ TO ambient @ NCRing.
      Example
         A = QQ{x,y,z,w}
	 B = A/ncIdeal{y*z + z*y - x^2, x*z + z*x - y^2, z^2 - x*y - y*x}
	 M = ncMatrix {{x,y,z,w},{y,z,w,x}}
	 N = M || 2*M || -3*M
	 ring N
	 ring lift N
///

doc ///
   Key
      NCRingElement
   Headline
      Type of an element in a noncommutative ring
   Description
      Text
        This is the type of an element in a noncommutative graded ring.  One can deal with these elements
	in much the same way as in the commutative case.  See @ TO RingElement @ for details.
///

doc ///
   Key
      (degree, NCRingElement)
   Headline
      Returns the degree of an NCRingElement
   Usage
     d = degree f
   Inputs
     f : NCRingElement
   Outputs
     d : ZZ
   Description
      Text
        Returns the degree of an NCRingElement.  At the moment, multigraded NCRings are not supported.
      Example
        A = QQ{x,y,z,w}
        f = x^2+y^2+z^3
	degree f
        isHomogeneous f
	setWeights(A,{3,3,2,1})
	degree f
	isHomogeneous f
///

doc ///
   Key
      (ring, NCRingElement)
   Headline
      Returns the NCRing of an NCRingElement
   Usage
     A = ring f
   Inputs
     f : NCRingElement
   Outputs
     A : NCRing
   Description
      Text
        Returns the ring of an NCRingElement
      Example
        A = QQ{x,y,z,w}
        f = x^2+y^2+z^3
        ring f
///

doc ///
   Key
      (terms, NCRingElement)
   Headline
      Returns the terms of an NCRingElement
   Usage
     t = terms f
   Inputs
     f : NCRingElement
   Outputs
     t : List
   Description
      Text
        Returns the list of terms that make up the NCRingElement.  It is a list of
	NCRingElements.
      Example
        A = QQ{x,y,z,w}
        f = 2*x^2+y^2+z^3
        t = terms f
        first t
///

doc ///
   Key
      (size, NCRingElement)
   Headline
      Returns the number of terms of an NCRingElement
   Usage
     n = size f
   Inputs
     f : NCRingElement
   Outputs
     n : ZZ
   Description
      Text
        Returns the number of terms of an NCRingElement.
      Example
        A = QQ{x,y,z,w}
        f = x^2+y^2+z^3
        size f
///

doc ///
   Key
      (support, NCRingElement)
   Headline
      Returns the variables appearing in the NCRingElement
   Usage
     sup = support f
   Inputs
     f : NCRingElement
   Outputs
     sup : List
   Description
      Text
        Returns the variables appearing in f (as elements of the ring of f).
      Example
        A = QQ{x,y,z,w}
        f = x^2+y^2+z^3
        sup = support f
	first sup
///

doc ///
   Key
      (monomials, NCRingElement)
   Headline
      Returns the monomials appearing in the NCRingElement
   Usage
     mons = support f
   Inputs
     f : NCRingElement
   Outputs
     mons : NCMatrix
   Description
      Text
        Returns the monomials appearing in NCRingElement as an NCMatrix.
      Example
        A = QQ{x,y,z,w}
        f = x^2+y^2+z^3
        mons = monomials f
///

doc ///
   Key
      (leadMonomial, NCRingElement)
   Headline
      Returns the lead monomial of an NCRingElement
   Usage
     mon = leadMonomial f
   Inputs
     f : NCRingElement
   Outputs
     mon : NCRingElement
   Description
      Text
        Returns the lead monomial of an NCRingElement (as an NCRingElement).
      Example
        A = QQ{x,y,z,w}
        f = x^2+y^2+z^3
        mon = leadMonomial f
///

doc ///
   Key
      (leadCoefficient, NCRingElement)
   Headline
      Returns the lead monomial of an NCRingElement
   Usage
     coeff = leadCoefficient f
   Inputs
     f : NCRingElement
   Outputs
     coeff : RingElement
   Description
      Text
        Returns the lead coefficient of an NCRingElement (as an element of the base).
      Example
        A = QQ{x,y,z,w}
        f = x^2+y^2+z^3
        coeff = leadCoefficient f
///

doc ///
   Key
      (leadTerm, NCRingElement)
   Headline
      Returns the lead term of an NCRingElement
   Usage
     coeff = leadTerm f
   Inputs
     f : NCRingElement
   Outputs
     coeff : NCRingElement
   Description
      Text
        Returns the lead term of an NCRingElement (as an NCRingElement).
      Example
        A = QQ{x,y,z,w}
        f = x^2+y^2+2*z^3
        coeff = leadTerm f
///

doc ///
   Key
     (isConstant, NCRingElement)
   Headline
     Returns whether the NCRingElement is constant
   Usage
     t = isConstant f
   Inputs
     f : NCRingElement
   Outputs
     t : Boolean
   Description
      Text
        Returns whether the NCRingElement is constant.
      Example
        A = QQ{x,y,z,w}
        f = x^2+y^2+2*z^3
        isConstant f
	g = promote(1,A)
	isConstant g
///

doc ///
   Key
     (baseName, NCRingElement)
   Headline
     Returns the base name of a generator of an NCRing
   Usage
     name = baseName f
   Inputs
     f : NCRingElement
   Description
      Text
        Returns the base name of a generator of an NCRing.  This could be something of type
	@ TO IndexedVariable @ or a @ TO Symbol @.
      Example
        A = QQ{x,y,z,w}
        baseName x
	B = QQ{p_1..p_6}
	baseName p_1
///

doc ///
   Key
      (toString, NCRingElement)
   Headline
      Converts an NCRingElement to a string
   Usage
     str = toString f
   Inputs
     f : NCRingElement
   Outputs
     str : String
   Description
      Text
        Converts an NCRingElement to a string.  This should be readable by both Macaulay2 as well as Bergman.
      Example
        A = QQ{x,y,z,w}
        f = x^2+y^2+z^3
        toString f
///

doc ///
   Key
      (symbol *, List, NCRingElement)
   Headline
      Scales a list by an NCRingElement on the right
   Usage
     xsf = xs*f
   Inputs
     xs : List
     f : NCRingElement
   Outputs
     xsf : List
   Description
      Text
        Scales a list by an NCRingElement on the right.
      Example
        A = QQ{x,y}
        f = x^2+y^2
        bas = flatten entries basis(3,A)
	bas*f
///

doc ///
   Key
      (symbol *, NCRingElement, List)
   Headline
      Scales a list by an NCRingElement on the left
   Usage
     fxs = f*xs
   Inputs
     f : NCRingElement
     xs : List
   Outputs
     xsf : List
   Description
      Text
        Scales a list by an NCRingElement on the right.
      Example
        A = QQ{x,y}
        f = x^2+y^2
        bas = flatten entries basis(3,A)
	f*bas
///

doc ///
   Key
      NCGroebnerBasis
   Headline
      Type of a Groebner basis for an NCIdeal in an NCRing.
   Description
     Text
       This is the type for a Groebner basis of an ideal in the tensor algebra.
       One can provide one using the @ TO [ncGroebnerBasis,InstallGB] @ option of @ TO ncGroebnerBasis @
       if you happen to know it.
       
       One also can have Macaulay2 call Bergman and have it computed via the function
       @ TO twoSidedNCGroebnerBasisBergman @.  This command is automatically called when defining
       a quotient ring, if the defining ideal does not yet have a cached Groebner basis.
       
       You can also install one from a Bergman output file if you have that handy; see
       @ TO gbFromOutputFile @.
       
       Below are a couple of examples.
     Example
       R = QQ[a,b,c,d]/ideal{a*b+c*d}
       A = R {x,y,z}
       I = ncIdeal {a*x*y,b*z^2}
       Igb = ncGroebnerBasis(I, InstallGB=>true)
     Text
       Note that after the InstallGB flag is set, no checking is done
       to ensure that the input is in fact a Groebner basis.
     Example
       c*z^2 % Igb 
       b*z^2 % Igb
       A = QQ{x,y,z}
       p = y*z + z*y - x^2
       q = x*z + z*x - y^2
       r = z^2 - x*y - y*x
       I = ncIdeal {p,q,r}
       Igb = ncGroebnerBasis I
       normalFormBergman(z^17,Igb)
     Text
       stuff
///

doc ///
   Key
      ncGroebnerBasis
      (ncGroebnerBasis,List)
      (ncGroebnerBasis,NCIdeal)
      [ncGroebnerBasis,InstallGB]
   Headline
      Compute a noncommutative Groebner basis.
   Usage
     Igb = ncGroebnerBasis I
   Inputs
     I : NCIdeal
         or a @ TO List @ of NCRingElements
     DegreeLimit => ZZ
                  the maximum degree for the calculation of a Groebner basis
     InstallGB => Boolean
                  set this to true to install a NCGroebnerBasis without verifying that 
		  it is one
   Outputs
     Igb : NCGroebnerBasis
   Description
     Example
       R = QQ[a,b,c,d]/ideal{a*b+c*d}
       A = R {x,y,z}
       I = ncIdeal {a*x*y,b*z^2}
       Igb = ncGroebnerBasis(I, InstallGB=>true)
       c*z^2 % Igb 
       b*z^2 % Igb
     Text
       Note that after the InstallGB flag is set, no checking is done
       to ensure that the input is in fact a Groebner basis.
     Example
       A = QQ{x,y,z}
       p = y*z + z*y - x^2
       q = x*z + z*x - y^2
       r = z^2 - x*y - y*x
       I = ncIdeal {p,q,r}
       Igb = ncGroebnerBasis I
     Text
       If the InstallGB flag is not set, then a call to Bergman is made, if the base ring is @ TO QQ @ or a finite field
       of characteristic p.  Otherwise, an error is raised.
       
       Now we can do things with an ncgb, like compute normal forms (using the Bergman interface).
     Example
       normalFormBergman(z^17,Igb)
     Text
       Or using the built in reduction code:
     Example
       z^17 % Igb
     Text
       Calls to Bergman are usually faster, except for when the polynomial is small.  See the documentation
       for @ TO (symbol %, NCRingElement, NCGroebnerBasis) @ for details on controlling when Bergman is called.
///

-*

-- This block of doc nodes is in case we decide to have stubs for the name of the option
-- in addition to the description of how to use the option in the function call.

doc ///
   Key
      CacheBergmanGB
   Headline
      Whether or not to cache the gb from Bergman to a file for later use
   SeeAlso
      gbFromOutputFile
      twoSidedNCGroebnerBasisBergman
///

doc ///
   Key
      MakeMonic
   Headline
      An option that specifies Bergman output should be made monic
   SeeAlso
      gbFromOutputFile
      twoSidedNCGroebnerBasisBergman
///

doc ///
   Key
      ReturnIdeal
   Headline
      An option that specifies certain NCAlgebra functions should return an ideal rather than a Groebner basis.
   SeeAlso
      gbFromOutputFile
///

doc ///
   Key
      Derivation
   Headline
      An option that specifies that an NCRingMap be considered a derivation.
   SeeAlso
      ncMap
///

doc ///
   Key
      NumModuleVars
   Headline
      An option specifying the number of module variables in the ring of the Groebner basis.
///

doc ///
   Key
      InstallGB
   Headline
      Install a NCGroebnerBasis (without verifying that it is one).
   SeeAlso
      ncGroebnerBasis
///

*-

doc ///
   Key
      gbFromOutputFile
      (gbFromOutputFile,NCPolynomialRing,String)
      [gbFromOutputFile,CacheBergmanGB]
      [gbFromOutputFile,ReturnIdeal]
      [gbFromOutputFile,MakeMonic]
   Headline
      Read in a NCGroebnerBasis from a Bergman output file.
   Usage
      Igb = gbFromOutputFile(A,fileName)
   Inputs
      A : NCPolynomialRing
      fileName : String
      CacheBergmanGB => Boolean
                    specifies whether or not to cache the Groebner basis for later use
      MakeMonic => Boolean
                   specifies whether the Bergman output should be made monic
      ReturnIdeal => Boolean
                     specifies whether to return an NCIdeal or the NCGroebnerBasis
   Outputs
      Igb : NCGroebnerBasis
            or an @ TO NCIdeal @
   Description
      Text
        This command reads in a Groebner basis from a Bergman output file.
	It can be useful if you have performed a lengthy computation before,
	and wish to load in a previously computed result to do some computations.
      
        The Groebner basis we are reading in for this example is located in the NCAlgebra
	auxiliary files directory. We are currently unable to compile this documentation 
	node in a way that enables M2 to read the file. We give a text version of the
	session until the issue is resolved.
      Example
        A=QQ{x56,x46,x36,x26,x16,x45,x35,x25,x15,x34,x24,x14,x23,x13,x12}
      Text
	I = gbFromOutputFile(A,"NCAlgebra/FK.bo", ReturnIdeal=>true)
      Text
	B=A/I
	-- The gb in the file is up to degree 6
      Text
        hilbertSeries(B,Order=>4)	
      Text
        In "Quadratic algebras, Dunkl elements, and Schubert calculus," Fomin and Kirillov
        introduce a family of noncommutative algebras E_n which may be thought of as the 
        quadratic closure of the ring of "Bruhat operators" acting on a vector space whose 
        basis is the symmetric group. In that paper, they provide Hilbert series for the 
        algebras E_1, ..., E_5. The file loaded above is a Groebner basis of E_6 to degree 6.
        Thus B is isomorphic to E_6 in low degrees.
///

doc ///
  Key
    (generators, NCGroebnerBasis)
  Headline
    The list of algebra generators of an NCGroebnerBasis
  Usage
    gensIgb = generators Igb
  Inputs
    Igb : NCGroebnerBasis
  Outputs
    gensIgb : List
  Description
    Text
       This function returns the generators of an NCGroebnerBasis as a list.  As usual,
       gens is a synonym for generators.
    Example
       A = QQ{x,y,z}
       p = y*z + z*y - x^2
       q = x*z + z*x - y^2
       r = z^2 - x*y - y*x
       I = ncIdeal {p,q,r}
       Igb = ncGroebnerBasis I
       gens Igb
///

doc ///
   Key
      (symbol %, NCRingElement, NCGroebnerBasis)
      (symbol %, QQ, NCGroebnerBasis)
      (symbol %, ZZ, NCGroebnerBasis)
   Headline
      Reduces a NCRingElement by a NCGroebnerBasis
   Usage
     fred = f % Igb
   Inputs
     f : NCRingElement
         or an integer, or a rational number
     Igb : NCGroebnerBasis
   Outputs
     fred : NCRingElement
   Description
     Text
       This command reduces the input modulo a noncommutative Groebner basis.
       It will either reduce it using top-level Macaulay code, or via a call to
       Bergman, depending on the size and degree of the input element.
     Example
       A = QQ{x,y,z}
       p = y*z + z*y - x^2
       q = x*z + z*x - y^2
       r = z^2 - x*y - y*x
       I = ncIdeal {p,q,r}
       Igb = ncGroebnerBasis I
       z^6 % Igb
///

doc ///
   Key
      twoSidedNCGroebnerBasisBergman
      (twoSidedNCGroebnerBasisBergman,List)
      (twoSidedNCGroebnerBasisBergman,NCIdeal)
      [twoSidedNCGroebnerBasisBergman,DegreeLimit]
      [twoSidedNCGroebnerBasisBergman,NumModuleVars]
      [twoSidedNCGroebnerBasisBergman,CacheBergmanGB]
      [twoSidedNCGroebnerBasisBergman,MakeMonic]
   Headline
      Calls Bergman to compute a two sided noncommutative Groebner Basis.
   Usage
      twoSidedNCGroebnerBasisBergman I
   Inputs
      I : NCIdeal
          or a @ TO List @ of NCRingElements
      DegreeLimit => ZZ
                     specifies the maximum degree for the Groebner basis calculation
      NumModuleVars => ZZ
                     specifies the number of module variables in the ring of the Groebner basis
      MakeMonic => Boolean
                   specifies whether the Bergman output should be made monic
      CacheBergmanGB => Boolean
                    specifies whether or not to cache the Groebner basis for later use
   Outputs
      : NCGroebnerBasis
   Description
     Text
        This command calls the computer algebra system Bergman to
	compute a noncommutative Groebner basis.
	
	Since Groebner bases in the tensor algebra need not be
	finitely generated, one should specify a degree limit on the
	computation unless one has a reason to believe the Groebner
	basis is finite.
     Example
       A = QQ{x,y,z}
       p = y*z + z*y - x^2
       q = x*z + z*x - y^2
       r = z^2 - x*y - y*x
       I = ncIdeal {p,q,r}
       Igb = twoSidedNCGroebnerBasisBergman I
///

doc ///
   Key
      NCLeftIdeal
   Headline
      Type of a left ideal in a noncommutative ring
   Description
      Text
         This defines a left ideal in a noncommutative ring.  Not much
	 can be done with these objects at this point (as one can tell
	 by the dearth of operations that take an NCLeftIdeal as
	 input), , but eventually it will be a 'fully featured'
	 object.
///

doc ///
   Key
      ncLeftIdeal
      (ncLeftIdeal, List)
      (ncLeftIdeal, NCRingElement)
   Headline
      Define a left ideal in a noncommutative ring
   Usage
      I = ncLeftIdeal fs
   Inputs
      fs : NCRingElement
           or a @ TO List @ of NCRingElements
   Outputs
      I : NCLeftIdeal      
   Description
      Text
         This defines a left ideal in a noncommutative ring.  Not much
	 can be done with these objects at this point (as one can tell
	 by the dearth of operations that take an NCLeftIdeal as
	 input), but eventually it will be a 'fully featured'
	 object.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncLeftIdeal{p,q,r}
///

doc ///
   Key
      (generators, NCLeftIdeal)
   Headline
      Returns the generators of an NCLeftIdeal
   Usage
      gensI = generators I
   Inputs
      I : NCLeftIdeal      
   Outputs
      gensI : List
   Description
      Text
         Returns the generators of an NCLeftIdeal.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncLeftIdeal{p,q,r}
	 gens I
///

doc ///
   Key
      (ring, NCLeftIdeal)
   Headline
      Returns the ring of an NCLeftIdeal
   Usage
      A = ring I
   Inputs
      I : NCLeftIdeal      
   Outputs
      A : NCRing
   Description
      Text
         Returns the ring of an NCLeftIdeal.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncLeftIdeal{p,q,r}
         ring I
///

doc ///
   Key
      (symbol +, NCLeftIdeal, NCLeftIdeal)
   Headline
      Sum of NCLeftIdeals
   Usage
      K = I + J
   Inputs
      I : NCLeftIdeal      
      J : NCLeftIdeal      
   Outputs
      K : NCLeftIdeal
   Description
      Text
         This command sums two NCLeftIdeals.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncLeftIdeal{p,q}
         J = ncLeftIdeal r
	 I + J
///

doc ///
   Key
      NCRightIdeal
   Headline
      Type of a right ideal in a noncommutative ring
   Description
      Text
         This defines a right ideal in a noncommutative ring.  Not much
	 can be done with these objects at this point (as one can tell
	 by the dearth of operations that take an NCRightIdeal as
	 input), , but eventually it will be a 'fully featured'
	 object.
///

doc ///
   Key
      ncRightIdeal
      (ncRightIdeal, List)
      (ncRightIdeal, NCRingElement)
   Headline
      Define a right ideal in a noncommutative ring
   Usage
      I = ncRightIdeal fs
   Inputs
      fs : NCRingElement
           or a @ TO List @ of NCRingElements
   Outputs
      I : NCRightIdeal      
   Description
      Text
         This defines a right ideal in a noncommutative ring.  Not much
	 can be done with these objects at this point (as one can tell
	 by the dearth of operations that take an NCRightIdeal as
	 input), but eventually it will be a 'fully featured'
	 object.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncRightIdeal{p,q,r}
///

doc ///
   Key
      (generators, NCRightIdeal)
   Headline
      Returns the generators of an NCRightIdeal
   Usage
      gensI = generators I
   Inputs
      I : NCRightIdeal      
   Outputs
      gensI : List
   Description
      Text
         Returns the generators of an NCRightIdeal.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncRightIdeal{p,q,r}
	 gens I
///

doc ///
   Key
      (ring, NCRightIdeal)
   Headline
      Returns the ring of an NCRightIdeal
   Usage
      A = ring I
   Inputs
      I : NCRightIdeal      
   Outputs
      A : NCRing
   Description
      Text
         Returns the ring of an NCRightIdeal.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncRightIdeal{p,q,r}
         ring I
///

doc ///
   Key
      (symbol +, NCRightIdeal, NCRightIdeal)
   Headline
      Sum of NCRightIdeals
   Usage
      K = I + J
   Inputs
      I : NCRightIdeal      
      J : NCRightIdeal      
   Outputs
      K : NCRightIdeal
   Description
      Text
         This command sums two NCRightIdeals.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncRightIdeal{p,q}
         J = ncRightIdeal r
	 I + J
///

doc ///
   Key
      NCIdeal
   Headline
      Type of a two-sided ideal in a noncommutative ring
   Description
      Text
         This defines a right ideal in a noncommutative ring.
///

doc ///
   Key
      ncIdeal
      (ncIdeal, List)
      (ncIdeal, NCRingElement)
   Headline
      Define a two-sided ideal in a noncommutative ring
   Usage
      I = ncIdeal fs
   Inputs
      fs : NCRingElement
           or a @ TO List @ of NCRingElements
   Outputs
      I : NCIdeal      
   Description
      Text
         This defines a two-sided ideal in a noncommutative ring.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncIdeal{p,q,r}
///

doc ///
   Key
      (generators, NCIdeal)
   Headline
      Returns the generators of an NCIdeal
   Usage
      gensI = generators I
   Inputs
      I : NCIdeal      
   Outputs
      gensI : List
   Description
      Text
         Returns the generators of an NCIdeal.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncIdeal{p,q,r}
	 gens I
///

doc ///
   Key
      (ring, NCIdeal)
      (ring, NCGroebnerBasis)
   Headline
      Returns the ring of an NCIdeal or NCGroebnerBasis
   Usage
      A = ring I
   Inputs
      I : NCIdeal      
   Outputs
      A : NCRing
   Description
      Text
         Returns the ring of an NCIdeal.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncIdeal{p,q,r}
         ring I
///

doc ///
   Key
      (symbol +, NCIdeal, NCIdeal)
   Headline
      Sum of NCIdeals
   Usage
      K = I + J
   Inputs
      I : NCIdeal      
      J : NCIdeal      
   Outputs
      K : NCIdeal
   Description
      Text
         This command sums two NCIdeals.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncIdeal{p,q}
         J = ncIdeal r
	 I + J
///

doc ///
   Key
      (basis, ZZ, NCIdeal)
   Headline
      Returns a basis of an NCIdeal in a particular degree.
   Usage
      bas = basis(n,I)
   Inputs
      n : ZZ
      I : NCIdeal
   Outputs
      bas : NCMatrix
   Description
      Text
         This command returns a basis (or minimal generating set, if
	 the ground ring is not a field), of a homogeneous two-sided
	 ideal in a noncommutative ring.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncIdeal{p,q,r}
	 bas = basis(3,I)
///

doc ///
   Key
      (basis, ZZ, NCLeftIdeal)
   Headline
      Returns a basis of an NCLeftIdeal in a particular degree.
   Usage
      bas = basis(n,I)
   Inputs
      n : ZZ
      I : NCLeftIdeal
   Outputs
      bas : NCMatrix
   Description
      Text
         This command returns a basis (or minimal generating set, if
	 the ground ring is not a field), of a homogeneous left
	 ideal in a noncommutative ring.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncLeftIdeal{p,q,r}
	 bas = basis(3,I)
///

doc ///
   Key
      (basis, ZZ, NCRightIdeal)
   Headline
      Returns a basis of an NCRightIdeal in a particular degree.
   Usage
      bas = basis(n,I)
   Inputs
      n : ZZ
      I : NCRightIdeal
   Outputs
      bas : NCMatrix
   Description
      Text
         This command returns a basis (or minimal generating set, if
	 the ground ring is not a field), of a homogeneous right
	 ideal in a noncommutative ring.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncRightIdeal{p,q,r}
	 bas = basis(3,I)
///

doc ///
   Key
      (basis, ZZ, NCRing)
   Headline
      Returns a basis of an NCRing in a particular degree.
   Usage
      bas = basis(n,B)
   Inputs
      n : ZZ
      I : NCRing
   Outputs
      bas : NCMatrix
   Description
      Text
         This command returns a basis (or minimal generating set, if
	 the ground ring is not a field), of a graded noncommutative
         ring.
      Example
         A = QQ{x,y,z}
         p = y*z + z*y - x^2
         q = x*z + z*x - y^2
         r = z^2 - x*y - y*x
         I = ncIdeal{p,q,r}
	 B = A/I
	 bas = basis(4,B)
///

doc ///
   Key
     setWeights
     (setWeights,NCRing,List)
   Headline
      Set a nonstandard grading for a NCRing.
   Usage
      setWeights(A,degList)
   Inputs
      A : NCRing
      degList : List
                a list of integer weights to be assigned to the generators of A
   Outputs
      : NCRing
   Description
      Text
         This method enables the user to work with rings which are not naturally graded
	 (the generators are not all degree 1). The user should be aware that  methods
	 which call Bergman may not work with nonstandard gradings.
	 Perhaps the most important example is hilbertBergman, which throws an error. 
	 Instead, use @ TO hilbertSeries @. 
      Example
         A=QQ{x,y,z}
	 w=x^3-y^2
	 isHomogeneous w
     	 setWeights(A, {2,3,1})
	 isHomogeneous w

	 C = QQ{a,b,c}
	 g = ncMap(C,A,{a^3,b^2,a+b,a-b})	 
	 isHomogeneous g
	 setWeights(A,{3,2,1,1})
	 isHomogeneous g

   SeeAlso
      isHomogeneous
///

doc ///
   Key
      (isHomogeneous, NCIdeal)
      (isHomogeneous, NCRightIdeal)
      (isHomogeneous, NCLeftIdeal)
      (isHomogeneous, NCRing)
      (isHomogeneous, NCMatrix)
      (isHomogeneous, NCRingElement)
   Headline
      Determines whether the input defines a homogeneous object
   Usage
      isHomogeneous x
   Inputs
      x : NCIdeal
          or an @ TO NCLeftIdeal @ an @ TO NCRightIdeal @ an @ TO NCRing @ an
	  @ TO NCMatrix @ or an @ TO NCRingElement @
   Outputs
      : Boolean
   Description
      Text
         Many methods in the NCAlgebra package require inputs to be homogeneous. The 
	 meaning of "homogeneous" depens on the type of object. 
      Text
         If x is an @ TO NCRingElement @, the method returns true if all terms of x 
	 have the same degree.

         If x is an @ TO NCIdeal @, @ TO NCLeftIdeal @, or @ TO NCRightIdeal @, the 
	 method returns true if all generators of the ideal are homogeneous (not 
	 necessarily of the same degree). 
   	
	 If x is an @ TO NCPolynomialRing @, the method returns true. If x is any 
         other @ TO NCRing@, the method returns true if and only if the defining 
         ideal of x is homogeneous.
	
 	 If x is an @ TO NCMatrix @, the method returns true if integer weights were
	 assigned to the source and target of the associated map of free right modules
	 such that the map is graded (degree 0). See @ TO assignDegrees @.
      Example
         A=QQ{x,y,z}
	 w=x^3-y^2
	 isHomogeneous w
     	 setWeights(A, {2,3,1})
	 isHomogeneous w 
	 I = ncIdeal{w,x+z^2}
	 isHomogeneous I
      Example	
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
         M = ncMatrix {{x,y,z,0}, {-y*z-2*x^2,-y*x,z*x-x*z,x},{x*y-2*y*x,x*z,-x^2,y}, {-y^2-z*x,x^2,-x*y,z}}
         isHomogeneous M
         assignDegrees(M,{1,0,0,0},{2,2,2,1})
	 isHomogeneous M
         N = ncMatrix {gens B}
	 isHomogeneous N
   SeeAlso
       (isHomogeneous, NCRingMap)
       setWeights
       assignDegrees
///

doc ///
   Key
      assignDegrees
      (assignDegrees,NCMatrix)
      (assignDegrees,NCMatrix,List,List)
   Headline
      Weights entries of a matrix to make associated map of free modules graded
   Usage
      assignDegrees(M) or assignDegrees(M,targetDegs,sourceDegs)
   Inputs
      M : NCMatrix
      targetDegs : List
                   a list of integer weights for the target basis vectors
      sourceDegs : List
                   a list of integer weights for the source basis vectors
   Outputs
      : NCMatrix
        the same matrix M with source and target keys specified and isHomogeneous flag true
   Description
      Text
         A matrix M with homogeneous entries in an NCRing can determine a homomorphism
	 of graded free modules if the entries satisfy certain consistency conditions.
	 Finding a set of degrees for source and target basis vectors such that M
	 determines a graded module map is an integer programming problem. This method
	 does not solve the problem in general. 
	 
      Text
         With only one input, this method checks to see if the entries in each column
	 all have the same homogeneous degree. If so, the method assigns source and
	 target degrees in the natural way and the @ TO isHomogeneous @ flag is set for M.
	 If not, all degrees are set to 0 and the flag is not set. This method is called
	 any time a matrix is created.
	 
      Text
         With three inputs, the user may specify the desired degree shifts on inputs
	 and outputs such that the associated module map is graded. 
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
         M = ncMatrix {{x,y,z,0}, {-y*z-2*x^2,-y*x,z*x-x*z,x},{x*y-2*y*x,x*z,-x^2,y}, {-y^2-z*x,x^2,-x*y,z}}
         isHomogeneous M
         assignDegrees(M,{1,0,0,0},{2,2,2,1})
	 isHomogeneous M
         N = ncMatrix {gens B}
	 isHomogeneous N
   SeeAlso
      isHomogeneous
      rightKernelBergman
///      


doc ///
   Key
      rightKernelBergman
      rightKernelDegreeLimit
      (rightKernelBergman,NCMatrix)
      [rightKernelBergman,DegreeLimit]
   Headline
      Methods for computing kernels of matrices over noncommutative rings using Bergman
   Usage
      rightKernelBergman(M,DegreeLimit=>n)
   Inputs
      M : NCMatrix
          a homogeneous matrix interpreted as a map of free right modules
      DegreeLimit => ZZ
                     the maximum degree in which to compute the kernel
   Outputs
      : NCMatrix
         the kernel of the matrix (considered as a right module map) to degree n
   Description
      Text
         Let M be a matrix with homogeneous entries in an NCRing. If the degrees of the 
	 entries of M satisfy certain consistency conditions, one can define a 
	 graded homomorphism of free right modules via left multiplication by M. If 
	 isHomogeneous(M) returns true, these conditions have been verified for M and
	 M is a valid input for rightKernelBergman. Otherwise, an error is returned
	 stating that M is not homogeneous. To set the isHomogeneous flag to true,
	 use @ TO assignDegrees @.
	 
      Text	 
	 For valid inputs, this method computes the first n homogeneous components of 
	 the (right) kernel of the homomorphism determined by M. If n is not specified 
	 by the user, the default maximum degree is 10. The method returns a minimal 
	 set of generators for the kernel in these degrees.
	 
	 The results of this command are cached in the input matrix M in M.cache#rightKernel,
	 and the maximum degree used in this computation is in M.cache#rightKernelDegreeLimit.
	 
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
         A = ambient B
         g = -y^3-x*y*z+y*x*z+x^3
         C = A/(ideal B + ncIdeal g)
         M3 = ncMatrix {{x,y,z,0}, {-y*z-2*x^2,-y*x,z*x-x*z,x},{x*y-2*y*x,x*z,-x^2,y}, {-y^2-z*x,x^2,-x*y,z}}
         assignDegrees(M3,{1,0,0,0},{2,2,2,1})
         ker1M3 = rightKernelBergman(M3)
         M3*ker1M3 == 0
         ker2M3 = rightKernelBergman(ker1M3)
         ker1M3*ker2M3 == 0
         ker3M3 = rightKernelBergman(ker2M3)
         ker2M3*ker3M3 == 0
	 
   SeeAlso
      isHomogeneous
      assignDegrees
      rightKernel
///

doc ///
   Key
      isLeftRegular
      (isLeftRegular,NCRingElement,ZZ)
      isRightRegular
      (isRightRegular,NCRingElement,ZZ)
   Headline
      Determines if a given (homogeneous) element is regular in a given degree
   Usage
      isLeftRegular(x,n) or isRightRegular(x,n)
   Inputs
      x : NCRingElement
      n : ZZ
          the degree in which regularity is checked.
   Outputs
      : Boolean
   Description
      Text
         Given an element x in an NCRing, isLeftRegular returns true if a*x=0 implies
	 a=0 for all a in the specified homogeneous degree n. Likewise isRightRegular
	 returns true if x*a=0 implies a=0 for all elements a of degree n. The
	 method calls @ TO leftMultiplicationMap @ or @ TO rightMultiplicationMap @ as
	 appropriate and checks the kernel in the specified degree. 
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 g = -y^3-x*y*z+y*x*z+x^3
         isLeftRegular(g,6)
	 
	 C = QQ{x,y}
	 D = C/ncIdeal{x^2+x*y,y^2}
	 isLeftRegular(x,1)
	 isRightRegular(x,1)
	 
   SeeAlso
      leftMultiplicationMap
      rightMultiplicationMap         
///

doc ///
   Key
      isCentral
      (isCentral,NCRingElement)
      (isCentral,NCRingElement,NCGroebnerBasis)
   Headline
      Determines if an element is central
   Usage
      isCentral x or isCentral(x,ncgb)
   Inputs
      x : NCRingElement
      ncgb : NCGroebnerBasis
             a Groebner basis for the NCRing to which x belongs.
   Outputs
      : Boolean
   Description
      Text
         This method checks to see if a given NCRing element is central.
      Example
        B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
        g = -y^3-x*y*z+y*x*z+x^3
	h = x^2 + y^2 + z^2
        isCentral h
        isCentral g
   SeeAlso
      centralElements
///

doc ///
   Key
      centralElements
      (centralElements, NCRing, ZZ)
   Headline
      Finds central elements in a given degree
   Usage
      centralElements(A,n)
   Inputs
      A : NCRing
      n : ZZ
          the homogeneous degree in which to compute central elements
   Outputs
      : NCMatrix
   Description
      Text
         If the given NCRing has central elements of the specified degree, this method
	 returns a basis for the space of central elements in that degree.
      Example
        B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	centralElements(B,2)
        centralElements(B,3)
///

doc ///
   Key
      normalElements
      (normalElements, NCQuotientRing, ZZ, Symbol, Symbol)
   Headline
      Finds normal elements
   Usage
      normalElements(A,n,x,y)
   Inputs
      A : NCQuotientRing
      n : ZZ
      x : Symbol
      y : Symbol
   Outputs
      : List
   Description
      Text
         Let b_1,...,b_n be a monomial basis for an NCRing A in degree d. We assume A
	 is generated by elements a_1,...,a_k of degree 1. A homogeneous element r in A
	 is normal if a_i*r is in the span of the r*a_j for all i.
      Text
         Using the input symbols x and y, we define the "normal variety" to be the 
	 set of common solutions to the equations  
	 x_j*a_i*b_j = y_j1*b_j*a_1+...+y_jk*b_j*a_k 
	 for all i and j. Saturating the ideal at each x_i we extract polynomial equations
	 the x_i must satisfy for the element x_1*b_1+...+x_n*b_n to be normal in A.
      Text
         Before computing the normal variety, this method checks for normal monomials
	 in degree n. These are returned first to reduce the complexity of the problem.
	 Then the method computes the variety and returns its components. The equations
         the method returns are given in terms of the indexed variable x. The indices are
	 basis monomials in degree n.
      Text
         The following example is a 3-dimensional Sklyanin algebra.
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 basis(2,B)
	 normalElements(B,2,r,s)
      Text
         The normal elements in degree 2 are x^2, y^2 and z^2. The basis
	 calculation shows x^2 and y^2 are normal forms in B. The normalElements
	 method first checks all basis monomials using @ TO isNormal @. In this case
	 it finds x^2 and y^2 are normal and returns this information. However,  
	 z^2 is not a normal form expression. The normal form of z^2 is x*y+y*x. In 
	 the second phase of the calculation, the method returns generators of the
	 ideal describing the normal elements (excluding the normal monomials). We
	 see the coefficients of basis monomials y*z and x*z must be 0 and the 
	 coefficients of x*y and y*x must be equal. The last equation identifies
	 z^2 = x*y+y*x as a normal element of degree 2.
      Example
         normalElements(B,3,t,u) 
	 g = -y^3-x*y*z+y*x*z+x^3
	 isCentral g
      Text
         In degree 3, there are no normal monomials. The function returns several equations
	 which determine the only normal element of degree 3 (up to scaling) is the central
	 element g.
///

doc ///
   Key
      (normalElements, NCRingMap, ZZ)
   Headline
      Finds elements normalized by a ring map
   Usage
      normalElements(f,n)
   Inputs
      f : NCRingMap
      n : ZZ
          a homogeneous degree in which to search for normal elements
   Outputs
      : List
   Description
      Text
         A normal element x in an NCRing R determines an automorphism f of R by
	 a*x=x*f(a). Conversely, given a ring endomorphism, we may ask if any x
	 satisfy the above equation for all a. 
      Text
         Given an NCRingMap f and a degree n, this method returns solutions to 
	 the equations a*x=x*f(a) for all generators a of R.
      Example
         B = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w})
	 sigma = ncMap(B,B,{y,z,w,x})
	 C = oreExtension(B,sigma,a)
	 sigmaC = ncMap(C,C,{y,z,w,x,a})
	 normalElements(sigmaC,1)
         normalElements(sigmaC,2)
         normalElements(sigmaC @@ sigmaC,2)
       
///


doc ///
   Key
      (isNormal, NCRingElement)
   Headline
      Determines if a given NCRingElement is normal
   Usage
      isNormal x
   Inputs
      x : NCRingElement
   Outputs
      : Boolean
   Description
      Text
         Given an element x in an NCRing R, this method returns true if Rx=xR.
      Example
         A = QQ{a,b,c}
	 I = ncIdeal {a*b+b*a,a*c+c*a,b*c+c*b}
	 B = A/I
	 sigma = ncMap(B,B,{b,c,a})
	 isWellDefined sigma
	 C = oreExtension(B,sigma,w)
	 isCentral w
	 isNormal w      
   SeeAlso
      isCentral
      normalElements
///

doc ///
   Key
      normalAutomorphism
      (normalAutomorphism,NCRingElement)
   Headline
      Computes the automorphism determined by a normal homogeneous element
   Usage
      normalAutomorphism x
   Inputs
      x : NCRingElement
          a homogeneous normal element
   Outputs
      : NCRingMap
   Description
      Text
         Let x be a homogeneous element in an NCRing R. If x is normal then x determines
	 a graded ring automorphism f of R by x*a = f(x)*a. This method returns this 
	 automorphism as an NCRingMap. 
      Example
         A = QQ{a,b,c}
	 I = ncIdeal {a*b+b*a,a*c+c*a,b*c+c*b}
	 B = A/I
	 sigma = ncMap(B,B,{b,c,a})
	 isWellDefined sigma
	 C = oreExtension(B,sigma,w)
	 isNormal w^2        
	 phi = normalAutomorphism w^2
	 matrix phi
	 (matrix sigma @@ sigma)
   SeeAlso
      normalElements	 
///

doc ///
   Key
      leftMultiplicationMap
      (leftMultiplicationMap,NCRingElement,ZZ)
      (leftMultiplicationMap,NCRingElement,ZZ,ZZ)
      (leftMultiplicationMap,NCRingElement,List,List)
      rightMultiplicationMap
      (rightMultiplicationMap,NCRingElement,ZZ)
      (rightMultiplicationMap,NCRingElement,ZZ,ZZ)
      (rightMultiplicationMap,NCRingElement,List,List)
   Headline
      Computes a matrix for left or right multiplication by a homogeneous element
   Usage
      leftMultiplicationMap(r,n) or leftMultiplicationMap(r,n,m) or leftMultiplicationMap(r,fromBasis,toBasis)
   Inputs
      r : NCRingElement
      n : ZZ
          the homogeneous degree for the source of the map
      m : ZZ
      	  the homogeneous degree for the target of the map
      fromBasis : List
                a list of monomials of the same homogeneous degree
      toBasis : List
                  a list of monomials of homogeneous degree deg(r) larger than the degree of the toBasis
   Outputs
      : Matrix
   Description
      Text
         These methods return a matrix over the coefficient ring of the NCRing to which r
	 belongs. The matrix represents left or right multiplication by r. Most commonly, 
	 the user will enter the ring element (required to be homogeneous) and a degree n.
	 The result is the matrix of the map A_n -> A_n+d where d is the degree of r.
	 The matrix is computed relative to the monomial basis obtain using 
	 @ TO (basis, ZZ, NCRing) @. 
	 
	 Alternatively, the user can enter sets of independent monomials to serve as a
	 basis for the domain and co-domain of the maps. The method left or right 
	 multiplies r by the fromBasis and converts to coordinates via @ TO sparseCoeffs @
	 and the toBasis.
	 
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 leftMultiplicationMap(x,2)
	 kernel oo
	 isRightRegular(x,2)
      Text
         If the element is not regular, you can use these methods to compute the 
	 annihilators in particular degrees.
      Example
	 C = QQ{x,y}
	 D = C/ncIdeal{x^2+x*y,y^2}
	 isRightRegular(x,1)
	 leftMultiplicationMap(x,1)
	 M=matrix gens kernel oo
	 basis(1,D)*M
   SeeAlso
      sparseCoeffs 
///

doc ///
   Key
      rightKernel
      (rightKernel,NCMatrix,ZZ)
      [rightKernel,NumberOfBins]
      [rightKernel,Verbosity]
   Headline
      Method for computing kernels of matrices over noncommutative rings in a given degree without using Bergman
   Usage
      rightKernel(M,n)
   Inputs
      M : NCMatrix
      n : ZZ
          the degree in which to compute the kernel
      NumberOfBins => ZZ
                      an integer dividing the number of rows of M
      Verbosity => ZZ
   Outputs
      : NCMatrix
   Description
      Text
         The method @ TO rightKernelBergman @ is a very effective tool for computing kernels of
	 homogeneous matrices with entries in rings over QQ or ZZ/p. This method 
	 provides an alternative that can be used for NCMatrices over any ground ring.
	 The method is also useful when one knows additional homological information -  
	 for example if the cokernel of M has a linear free resolution.
      Text
         Given an NCMatrix M and an integer n, this method returns a basis for the kernel
	 of the matrix (viewed as a linear map of free right modules) in homogeneous degree
	 n. The method successively computes annihilators of columns of M and intersects them. 
	 For large matrices or large values of n, it may save memory to break the calculation
	 into smaller pieces. Use the option NumberOfBins to reduce the memory (but increase
	 the time) the program uses. Set Verbosity to 1 to see progress updates.
      Text
         To avoid accidental calls to Bergman for normal form calculations, set the MAXSIZE
	 environment variable fairly high, say 1000.
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
         sigma = ncMap(B,B,{y,z,x})
	 C = oreExtension(B,sigma,w)
	 D = (ambient C)/(ideal C + ncIdeal{promote(w^2,ambient C)})
      Text
         This algebra is an Ore extension of a 3-dimensional Sklyanin algebra, factored
	 by the normal regular element w^2. This algebra is Koszul, hence it has a linear
	 free resolution. The rightKernel method is significantly faster than 
	 rightKernelBergman in this case. Also note the two methods return different
	 generating sets for the kernel.
      Example
	 M1 = ncMatrix {{x,y,z,w}}
	 M2 = rightKernel(M1,1)
	 M3 = rightKernel(M2,1)
	 rightKernelBergman(M2)
	 M4 = rightKernel(M3,1)
	 rightKernelBergman(M3)
   SeeAlso
      rightKernelBergman	 
///

doc ///
   Key
      quadraticClosure
      (quadraticClosure,NCIdeal)
      (quadraticClosure,NCQuotientRing)
   Headline
      Creates the subideal generated by quadratic elements of a given ideal
   Usage
      quadraticClosure I
   Inputs
      I : NCIdeal
   Outputs
      : NCIdeal
        the quadratic closure of I
   Description
      Text
         The quadratic closure of an NCIdeal in an NCPolynomialRing is the NCIdeal
	 generated by the elements of degree at most 2. Commonly used with 
	 Link to homogDual in the case where the ideal generators are homogeneous of
	 degree greater than 1.
	 
	 If the input is an NCQuotientRing, the method is applied to the defining
	 ideal of the quotient ring and the corresponding quotient ring is returned.
	 At the moment, quotients of quotients is not implemented, and the ambient
	 ring of the input NCQuotientRing is assumed to be an NCPolynomialRing.
	 
	 This method is commonly used in conjunction with @ TO homogDual @.
      Example
         A = QQ{x,y,z}
	 I = ncIdeal{x*z-z*x, y*z, x*y^2-y^2*x, x^3*y-y*x^3}
	 J = quadraticClosure I
   SeeAlso
      homogDual
///


doc ///
   Key
      homogDual
      (homogDual,Ring)
      (homogDual,NCIdeal)
      (homogDual,NCQuotientRing)
   Headline
      Computes the dual of a pure homogeneous ideal
   Usage
      homogDual I
   Inputs
      I : NCIdeal
          or an @ TO NCQuotientRing @
	  or a @ TO Ring @.
   Outputs
      : NCIdeal
           or an @ TO NCQuotientRing @
   Description
      Text
         The homogeneous dual of a pure ideal I in an NCPolynomialRing A is generated 
	 by the orthogonal complement to the generators of I under the natural pairing 
	 on the generating subspace of A and its linear dual. Though technically the dual
	 ideal belongs to the tensor algebra on the dual space of generators, this
	 method returns the dual ideal in the same NCPolynomialRing. 
	 
	 If the input is an NCQuotient ring, the method is applied to the defining
	 ideal of the quotient and the corresponding quotient ring is returned.
	 
	 Commonly used in conjunction with @ TO quadraticClosure @.
      Example
         A = QQ{x,y,z}
	 I = ncIdeal{x*z-z*x, y*z, x*y^2-y^2*x, x^3*y-y*x^3}
	 J = quadraticClosure I
         J' = homogDual J
   SeeAlso
      quadraticClosure
///

doc ///
   Key
      coordinates
      (coordinates, NCRingElement)
      (coordinates, List)
      [coordinates,Basis]
   Headline
      Computes coordinates relative to a given basis
   Usage
      coordinates L
   Inputs
      L : NCRingElement 
          or a @ TO List @ of NCRingElements
      Basis => List
               a list of homogeneous elements to use as a basis for computing coordinate vectors
   Outputs
      : Matrix
   Description
      Text
         Generally, linear algebra in graded rings is performed using the monomial basis
	 obtained from a Groebner basis calculation. In some cases, it is desirable to 
	 work relative to a different basis. This method calls @ TO sparseCoeffs @ to 
	 compute the coordinate vector(s) of a ring element (or a list of ring elements) 
	 relative to a user-specified basis.
	 If no basis is specified, the method simple calls @ TO sparseCoeffs @ with no
	 options.    
      Example
         R = QQ[w]/ideal(w^2+w+1)
	 A = skewPolynomialRing(R,promote(-1,R),{x,y,z})
      Text
         One motivating example comes from invariant theory. In this example, we take
	 a skew polynomial ring in three variables and act by the cyclic subgroup of graded
	 automorphisms of A generated by permuting the variables. A basis for the fixed ring
	 is given by "orbit sums" of basis monomials. Here we work in homogeneous degree 3.
      Example
	 g = ncMap(A,A,{y,z,x})
	 gList = {g, g^2, g^3}
	 a = sum apply(3,i-> (gList#i)(x^3))
	 b = sum apply(3,i-> (gList#i)(x^2*y))
	 c = sum apply(3,i-> (gList#i)(x*y^2))
	 d = sum apply(3,i-> (gList#i)(x*y*z))
      Text
         It is clear that these are linearly independent. Next, we take a homogeneous
	 polynomial of degree 3, make it invariant, and compute its coordinate vector.
      Example
         p = w^2*z^2*y+x^2*z+(1-w)*y^3
	 g(p)==p
	 p' = sum apply(3,i-> (gList#i)(p))
	 g(p')==p'
	 coordinates(p',Basis=>{a,b,c,d})
   SeeAlso
      sparseCoeffs
///   

doc ///
   Key
      sparseCoeffs
      (sparseCoeffs,List)
      (sparseCoeffs,NCRingElement)
      [sparseCoeffs,Monomials]
   Headline
      Converts ring elements into vectors over the coefficient ring
   Usage
      sparseCoeffs L
   Inputs
      L : NCRingElement
          or a @ TO List @ of NCRingElements
      Monomials => List
                   a list of monomials to use as a basis for computing coordinate vectors
   Outputs
      : Matrix
   Description
      Text 
         This method converts a list of ring elements to coordinate vectors - returned
	 as a matrix - relative to a list of monomilas. If the user does not 
	 supply a monomial list, the list is taken to be the monomials 
	 occurring in the elements of the list (with repetition).
      Example
         A=QQ{a, b, c, d, e, f, g, h}
	 F = a^2+b^2+c^2+d^2+e^2+f^2+g^2+h^2;
	 sparseCoeffs(F)
	 bas = flatten entries basis(2,A);
	 #bas
	 sparseCoeffs(F,Monomials=>bas)
	 sparseCoeffs(toList (10:F),Monomials=>bas)
///


doc ///
    Key
      NCRingMap
    Headline
      Type of a map to or from a noncommutative ring.
   Description
      Text
        As in the commutative case, a map F:R->S where R or S is an NCRing is specified by
 	giving the images in S of the variables of R. The target map is given first. 
      Text
         Common ways to make (and use) an NCRingMap include
      Code
         UL {TO (ncMap,Ring,NCRing,List),
	     TO (ncMap,NCRing,Ring,List),
	     TO (ncMap,NCRing,NCRing,List),
       	     TO (normalElements, NCRingMap, ZZ),
       	     TO (oreExtension,NCRing,NCRingMap,NCRingMap,NCRingElement),
	     TO (oreExtension,NCRing,NCRingMap,NCRingMap,Symbol),
	     TO (oreExtension,NCRing,NCRingMap,NCRingElement),
	     TO (oreExtension,NCRing,NCRingMap,Symbol),
       	     TO (oreIdeal,NCRing,NCRingMap,NCRingMap,NCRingElement),
	     TO (oreIdeal,NCRing,NCRingMap,NCRingMap,Symbol),
	     TO (oreIdeal,NCRing,NCRingMap,NCRingElement),
	     TO (oreIdeal,NCRing,NCRingMap,Symbol)
	     }
      Text
          Common ways to get information about NCRingMaps
       Code
         UL {TO (source,NCRingMap),
	     TO (target,NCRingMap),
	     TO (matrix,NCRingMap),
	     TO (isWellDefined,NCRingMap),
	     TO (isHomogeneous,NCRingMap),
	     TO (symbol _, NCRingMap, ZZ)}
	Text
	   Common operations involving NCRingMaps
	Code
	   UL {TO (ambient,NCRingMap),
	       TO (symbol /,List,NCRingMap),
	       TO (symbol SPACE, NCRingMap, NCRingElement),
     	       TO (symbol SPACE, NCRingMap, RingElement),
    	       TO (symbol SPACE, NCRingMap, NCMatrix),
	       }	
      Example
         A = skewPolynomialRing(QQ,(-1)_QQ,{w,x,y,z})
	 B = QQ{a,b,c}
	 f = ncMap(B,A,{a^3,b^2,a+b,a-b})
	 target f
	 source f
	 matrix f
      Text
         Note that NCRingMaps need not be well-defined or homogeneous.
         Apply a function to an element or a matrix using the usual function notation.
	 NCRingMaps are linear and multiplicative by definition.
      Example
         f(w*x+2*y)
	 isWellDefined f
	 isHomogeneous f
      Text
         The user has the option to define an NCRingMap to be a derivation. Of course,
	 such a map must have the same source and target.
      Example
     	 g = ncMap(B,B,{a*b,b^2,c*a*c},Derivation=>true)
	 g(a*b)==g(a)*b+a*g(b)
	 g(promote(1,B))
	 g(c*a+2*b)
///

doc ///
   Key
      ncMap
      (ncMap,NCRing,NCRing,List)
      (ncMap,Ring,NCRing,List)
      (ncMap,NCRing,Ring,List)
      [ncMap,Derivation]
   Headline
      Make a map to or from an NCRing
   Usage
      f = ncMap(B,A,m)
   Inputs
      A : NCRing
          or a @ TO Ring @
      B : NCRing
          or a @ TO Ring @
      m : List
          the images of the generators of A. 
      Derivation => Boolean
   Outputs
      f : NCRingMap
   Description
      Text 
	 NCRingMaps are linear and multiplicative by definition, but need not be
	 well-defined or homogeneous.
         The user has the option to define an NCRingMap to be a derivation. Such a 
	 map must have the same source and target.
      Example
         A = skewPolynomialRing(QQ,(-1)_QQ,{w,x,y,z})
	 B = QQ{a,b,c}
	 f = ncMap(B,A,{a^3,b^2,a+b,a-b})
	 f(w*x+2*y)
	 g = ncMap(B,B,{a*b,b^2,c*a*c},Derivation=>true)
	 g(a*b)==g(a)*b+a*g(b)
	 g(promote(1,B))
	 g(c*a+2*b)
   SeeAlso
      NCRingMap
///

doc ///
   Key
      (ambient, NCRingMap)
   Headline
      Extends an NCRingMap to the ambient ring of the source.
   Usage
      g = ambient f
   Inputs
      f : NCRingMap
   Outputs
      g : NCRingMap
   Description
      Text
         If f:R->S is a ring map and R=A/I is a quotient ring, this method
	 returns the NCRingMap g:A->S obtained by composing f with the natural
	 map. This method is called by @ TO (isWellDefined, NCRingMap) @ and can
	 be used to determine what ideal to mod out of the target so the map
	 becomes well-defined.
      Example
         A = skewPolynomialRing(QQ,(-1)_QQ,{w,x,y,z})
	 B = QQ{w,x,y,z}/ncIdeal{w*x+x*w,w*y+y*w,x*y+y*x}
	 f = ncMap(B,A,gens B)
	 isWellDefined f
	 g = ambient f
	 (gens ideal A)/g
///

doc ///
   Key
      (isHomogeneous, NCRingMap)
   Headline
      Determines if an NCRingMap preserves the natural grading
   Usage
     isHomogeneous f
   Inputs
     f : NCRingMap
   Outputs
       : Boolean
   Description
      Text
         This method returns true if the map f is degree 0; that is, if the 
	 generators of the source of f are mapped to elements of the same degree
	 in the target.
      Example
         A = skewPolynomialRing(QQ,(-1)_QQ,{w,x,y,z})
	 B = QQ{w,x,y,z}/ncIdeal{w*x+x*w,w*y+y*w,x*y+y*x}
	 f = ncMap(B,A,gens B)
	 isHomogeneous f
	 
	 C = QQ{a,b,c}
	 g = ncMap(C,A,{a^3,b^2,a+b,a-b})	 
	 isHomogeneous g
	 setWeights(A,{3,2,1,1})
	 isHomogeneous g
   SeeAlso
      (isHomogeneous, NCIdeal)   
///

doc ///
   Key
      (isWellDefined, NCRingMap)
   Headline
      Determines if an NCRingMap is well-defined.
   Usage
      isWellDefined f
   Inputs
      f : NCRingMap
   Outputs
      : Boolean
   Description
      Text
         Returns true if the given NCRingMap evaluates as 0 on the defining relations
	 of the source.
      Example
         A = skewPolynomialRing(QQ,(-1)_QQ,{w,x,y,z})
	 B = QQ{w,x,y,z}/ncIdeal{w*x+x*w,w*y+y*w,x*y+y*x}
	 f = ncMap(B,A,gens B)
	 isWellDefined f
	 
	 C = QQ{a,b,c}
	 g = ncMap(C,A,{a^3,b^2,a+b,a-b})	 
	 isWellDefined g
///

doc ///
   Key
      (symbol /, List, NCRingMap)
   Headline
      Applies an NCRingMap to each element of a list
   Usage
      L/f
   Inputs
      L : List
      f : NCRingMap
   Outputs
      : List
   Description
      Text
         This operation is the same thing as apply(L,x->f(x)). Note that the
	 operation is left associative.
      Example
         A = QQ{x,y}
	 f = ncMap(A,A,{x^2,y^2})
	 g = ncMap(A,A,{x+y,y})
	 gens A/f/g
	 gens A/g/f
///

doc ///
   Key
      (matrix, NCRingMap)
   Headline
      An NCMatrix associated to an NCRingMap.
   Usage
      matrix f
   Inputs
      f : NCRingMap
   Outputs
      : NCMatrix
   Description
      Text
         This function returns a matrix whose entries are the images of the
	 source generators in the target ring of f. The type of matrix depends on
	 the type of the target ring.
      Example
         A = skewPolynomialRing(QQ,(-1)_QQ,{w,x,y,z})
	 B = QQ[a,b,c,SkewCommutative=>true]
	 f = ncMap(B,A,{a^3,b^2,a+b,a-b})
	 matrix f
	 g = ncMap(A,A,{x,y,z,w})
	 matrix g
///

doc ///
   Key
      (symbol @@, NCRingMap, NCRingMap)
   Headline
      Compose two NCRingMaps
   Usage
      f @@ g
   Inputs
      f : NCRingMap
      g : NCRingMap
   Outputs
      : NCRingMap
   Description
      Example
         A = QQ{x,y}
	 f = ncMap(A,A,{x^2,y^2})
	 g = ncMap(A,A,{x+y,y})
	 gens A/f @@ g
	 gens A/g @@ f
///

doc ///
   Key
      (symbol SPACE, NCRingMap, NCRingElement)
      (symbol SPACE, NCRingMap, RingElement)
      (symbol SPACE, NCRingMap, NCMatrix)
   Headline
      Apply an NCRingMap to an element or matrix
   Usage
      f x
   Inputs
      f : NCRingMap
      x : NCRingElement
          or a @ TO RingElement @ or an @ TO NCMatrix @
   Outputs
      : NCRingElement
   Description
      Example
         A = skewPolynomialRing(QQ,(-1)_QQ,{w,x,y,z})
	 B = QQ{a,b,c}
	 f = ncMap(B,A,{a^3,b^2,a+b,a-b})
	 f(w*x+2*y)
	 f basis(2,A)

///

doc ///
   Key
      (symbol _, NCRingMap, ZZ)
   Headline
      Matrix of one homogeneous component of an NCRingMap
   Usage
      f_n
   Inputs
      f : NCRingMap
      n : ZZ
   Outputs
      : Matrix
   Description
      Text
         This method returns the homogeneous degree n component of the ring map f.
	 The output is the matrix (over the coefficient ring of the target) of the 
	 component map relative to the monomial bases for the source and target. 
      Example
         A = skewPolynomialRing(QQ,(-1)_QQ,{w,x,y,z})
	 setWeights(A,{1,1,2,2})
	 f = ncMap(A,A,{x,w,z,y})
	 basis(1,A)
	 f_1
	 basis(2,A)
	 f_2 
///

doc ///
   Key
      (source, NCRingMap)
   Headline
      Source of a map
   Description
      Text
         Gives the source of an @ TO NCRingMap @.
   SeeAlso
   	(target,NCRingMap)
///

doc ///
   Key
      (target, NCRingMap)
   Headline
      Target of a map
   Description
      Text
         Gives the target of an @ TO NCRingMap @.
   SeeAlso
   	(source,NCRingMap)
///

doc ///
   Key
      oreExtension
      (oreExtension,NCRing,NCRingMap,NCRingMap,NCRingElement)
      (oreExtension,NCRing,NCRingMap,NCRingMap,Symbol)
      (oreExtension,NCRing,NCRingMap,NCRingElement)
      (oreExtension,NCRing,NCRingMap,Symbol)
   Headline
      Creates an Ore extension of a noncommutative ring
   Usage
      oreExtension(A,sigma,delta,x) or oreExtension(A,sigma,x)
   Inputs
      A : NCRing
      sigma : NCRingMap
      delta : NCRingMap
      x : NCRingElement
          or a @ TO Symbol @
   Outputs
      : NCQuotientRing
   Description
      Text
         This method calls @ TO oreIdeal @ and returns the associated
	 Ore extension as an NCQuotientRing.
      Example
         B = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w})
	 sigma = ncMap(B,B,{y,z,w,x})
	 C = oreExtension(B,sigma,a)
   SeeAlso
      oreIdeal
///

doc ///
   Key
      oreIdeal
      (oreIdeal,NCRing,NCRingMap,NCRingMap,NCRingElement)
      (oreIdeal,NCRing,NCRingMap,NCRingMap,Symbol)
      (oreIdeal,NCRing,NCRingMap,NCRingElement)
      (oreIdeal,NCRing,NCRingMap,Symbol)
   Headline
      Creates the defining ideal of an Ore extension of a noncommutative ring
   Usage
      oreIdeal(A,sigma,delta,x) or oreIdeal(A,sigma,x)
   Inputs
      A : NCRing
      sigma : NCRingMap
      delta : NCRingMap
      x : NCRingElement
          or a @ TO Symbol @
   Outputs
      : NCIdeal
   Description
      Text
         Given a ring A, an Ore extension of A by x is the quotient of the free
	 extension A<x> by the relations x*a - sigma(a)*x-delta(a) where sigma
	 is an automorphism of A and delta is a sigma-derivation. This method returns
	 the defining ideal (in the appropriate tensor algebra) of an Ore extension
	 of A by x. The current version assumes the sigma-derivation delta is 0. 
      Example
         B = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w})
	 sigma = ncMap(B,B,{y,z,w,x})
	 C = oreIdeal(B,sigma,a)
   SeeAlso
      oreExtension
///

doc ///
   Key
      endomorphismRing
      (endomorphismRing,Module,Symbol)
      endomorphismRingGens 
   Headline
      Methods for creating endomorphism rings of modules over a commutative ring
   Usage
      E = endomorphismRing(M,X)
   Inputs
      M : Module
      X : Symbol
          the base name for the indexed variables serving as generators for the output ring
   Outputs
      E : NCQuotientRing
   Description
      Text
         This method computes a presentation for the endomorphism ring of a module 
	 over a commutative ring R. The presentation is given as a quotient of an 
	 NCPolynomialRing with coefficients in R generated by indexed variables. The
	 second argument specifies the symbol to use for the indexed variables. The
	 presentation this method returns is unlikely to be minimal. Simplifications
	 to the presentation may be made with Link to minimizeRelations.  
      Example
         Q = QQ[a,b,c]
         R = Q/ideal{a*b-c^2}
         kRes = res(coker vars R, LengthLimit=>7)
         M = coker kRes.dd_5
         B = endomorphismRing(M,X)
         gensI = gens ideal B
         gensIMin = minimizeRelations(gensI)
      Text
         The endomorphisms are cached in the endomorphism ring and can be accessed
	 via the key endomorphismRingGens. We verify that X_3 is redundant:
      Example
         maps = B.cache.endomorphismRingGens
	 maps_3 == maps_0*maps_2

///

doc ///
   Key
      minimizeRelations
      (minimizeRelations,List)
      [minimizeRelations,Verbosity]
   Headline
      Minimizes a list of NCRingElements
   Usage
      mgens = minimizeRelations(gens)
   Inputs
      gens : List
   Outputs
      mgens : List
   Description
      Text
         Given a list of NCRingElements, this method produces a generating set for the
	 NCIdeal generated by the list which (possibly) uses fewer generators and/or 
	 fewer NCRing generators. Commonly used with Link to endomorphismRing. 
      Example
         Q = QQ[a,b,c,d]
         R = Q/ideal{a*b+c*d}
         kRes = res(coker vars R, LengthLimit=>7)
         M = coker kRes.dd_5
         B = endomorphismRing(M,Y)
         gensI = gens ideal B
         gensIMin = minimizeRelations(gensI)
///

doc ///
   Key
      skewPolynomialRing
      (skewPolynomialRing,Ring,Matrix,List)
   Headline
      Defines a skew polynomial ring via a skewing matrix
   Usage
      B = skewPolynomialRing(R,M,L)
   Inputs
      R : Ring
      M : Matrix
      L : List
   Outputs
      B : NCRing
   Description
      Text
         This method constructs a skew polynomial ring with coefficients in the ring R
	 and generators from the list L. A valid input matrix is a square matrix over R
	 with at least #L rows such that M_{ij} = M_{ji}^{(-1)} and M_{ii}=1. The relations of the 
	 resulting ring have the form g_i*g_j - M_{ij}*g_j*g_i. If R is a Bergman coefficient
	 ring, an NCGroebnerBasis is computed for B.   
      Example
         R = QQ[q]/ideal{q^4+q^3+q^2+q+1}
	 M = matrix{{1,q,q},{q^4,1,1},{q^4,1,1}}
         B = skewPolynomialRing(R,M,{x,y,z})
         x*y == q^4*y*x
	 N = matrix{{1,1,1,1},{1,1,1,1},{1,1,1,1},{1,1,1,1}}
	 C = skewPolynomialRing(QQ,promote(N,QQ), {a,b,c,d})
         isCommutative C
         isCommutative B
         Bop = oppositeRing B
         y*x == q^4*x*y
   SeeAlso
     oppositeRing
///

doc ///
   Key
      (skewPolynomialRing,Ring,RingElement,List)
      (skewPolynomialRing,Ring,QQ,List)
      (skewPolynomialRing,Ring,ZZ,List)
   Headline
      Defines a skew polynomial ring via a scaling factor
   Usage
      skewPolynomialRing(R,f,L)
   Inputs
      R : Ring
      f : RingElement
          or an integer or a rational number
      L : List
   Outputs
      : NCRing
   Description
      Text
         This method constructs a skew polynomial ring with coefficient ring R
	 and generators elements of L. The relations all have the form a*b - f*b*a
	 where a and b are in L. If R is a Bergman coefficient ring, an NCGroebnerBasis
	 is computed for B.      
      Example
         R = QQ[q]/ideal{q^4+q^3+q^2+q+1}
         A = skewPolynomialRing(R,promote(2,R),{x,y,z,w})
         x*y == 2*y*x
         B = skewPolynomialRing(R,q,{x,y,z,w})
         x*y == q*y*x
         Bop = oppositeRing B
         y*x == q*x*y

         C = skewPolynomialRing(QQ,2_QQ, {x,y,z,w})         
         x*y == 2*y*x
	 D = skewPolynomialRing(QQ,1_QQ, {x,y,z,w})
         isCommutative C
         isCommutative D
         Cop = oppositeRing C
         y*x == 2*x*y
   SeeAlso
       oppositeRing
       skewPolynomialRing      

///

doc ///
   Key
      threeDimSklyanin
      (threeDimSklyanin,Ring,List)
      (threeDimSklyanin,Ring,List,List)
   Headline
      Defines a three-dimensional Sklyanin with given parameters
   Usage
      threeDimSklyanin(R,params,varList)
   Inputs
      R       : Ring
      params  : List
      varList : List
      DegreeLimit => ZZ
   Outputs
      : NCRing
   Description
      Text
         This method constructs a three dimensional Sklyanin algebra with parameters from
	 the params list, and variables from varList (see @ HREF{"http:////arxiv.org//abs//1107.2953","here"} @).
	 If either list is not length three, then an error is thrown.  The generic 
	 such algebra does not have a finite Groebner basis, so the optional parameter
	 DegreeLimit has been defaulted to 5.  If only one list is provided, it is used
	 for the variable names, and a random choice for each parameter is chosen.
      
         The following example is a PI algebra, and has a finite Groebner basis.
      Example
         B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
         ncGroebnerBasis ideal B
      Text
         This is not generically true, however:
      Example
         C = threeDimSklyanin(QQ,{a,b,c})
	 ncGroebnerBasis ideal C
      Text
         In all cases, there is a degree three central regular element (a formula
	 for which is given in the paper referenced above).
      Example
         centralElements(B,3)
	 centralElements(C,3)
      Text
         These algebras also all AS-regular and as such have the same Hilbert
	 series as a commutative polynomial algebra in three variables, as we can see here:
      Example
         hilbertBergman B
	 hilbertBergman(C,DegreeLimit=>5)
///

doc ///
   Key
      fourDimSklyanin
      (fourDimSklyanin,Ring,List)
      (fourDimSklyanin,Ring,List,List)
   Headline
      Defines a four-dimensional Sklyanin with given parameters
   Usage
      fourDimSklyanin(R,params,varList)
   Inputs
      R       : Ring
      params  : List
      varList : List
      DegreeLimit => ZZ
   Outputs
      : NCRing
   Description
      Text
         This method constructs a three dimensional Sklyanin algebra with parameters from
	 the params list, and variables from varList (see @ HREF{"https://www.math.washington.edu/~smith/Research/Skly-survey.pdf","here"} @).
	 If either list is not the appropriate length, then an error is thrown.  The generic 
	 such algebra has a fairly complicated Groebner basis, so the optional parameter
	 DegreeLimit has been defaulted to 5.  If only one list is provided, it is used
	 for the variable names, and a random choice for each parameter is chosen.
      
      	 In order to not get a degenerate example, one should ensure that the
	 parameters provided satisfy \alpha + \beta + \gamma + \alpha\beta\gamma = 0.
	 This method does not check this condition, since the degenerate examples are
	 of interest as well.  If no parameters are provided, however a generic choice
	 of \alpha,\beta and \gamma satisfying the equation above are selected.
      Example
         C = fourDimSklyanin(QQ,{a,b,c,d})
	 ncGroebnerBasis ideal C
      Text
         In all nondegenerate cases, there is are two central elements of degree two which form
	 a regular sequence on the four dimensional Sklyanin (this was proven by Paul
	 Smith and Toby Stafford in a paper in Compositio.
      Example
         centralElements(C,2)
      Text
         These algebras also all AS-regular and as such have the same Hilbert
	 series as a commutative polynomial algebra in four variables, as we can see here:
      Example
         hilbertBergman(C, DegreeLimit => 6)
///

doc ///
   Key
      toM2Ring
      (toM2Ring,NCRing)
      [toM2Ring,SkewCommutative]
   Headline
     Compute the abelianization of an NCRing and returns a Ring.
   Usage
     S = toM2Ring R 
   Inputs
      R : NCRing
      SkewCommutative => Boolean
   Outputs
     S : Ring
   Description
      Text
         This method takes an NCRing and returns the quotient of a commutative polynomial
	 ring (or an exterior algebra, if SkewCommutative=>true) on the same generators 
	 by the defining relations of the input ring. 
      Example
         A = skewPolynomialRing(QQ,(-1)_QQ,{w,x,y,z})
	 x*y-y*x
	 w^2
         B = toM2Ring(A)
	 x*y-y*x
	 w^2
	 C = toM2Ring(A,SkewCommutative=>true)
	 x*y-y*x
	 w^2
   SeeAlso
      toNCRing

///

doc ///
   Key
      toNCRing
      (toNCRing,Ring)
   Headline
      Converts a Ring to an NCRing
   Usage
     S = toNCRing R 
   Inputs
      R : Ring
   Outputs
     S : NCRing
   Description
      Text
         This function converts commutative rings and quotients of 
	 exterior algebras created by Macaulay2 to the type of an NCRing. An
	 error is returned if the input ring has some commutative and some
	 skew-commutative generators.
      Example
         R = QQ[a,b,c,d]
	 I = ideal(a*d-b*c)
         S = R/I
	 S' = toNCRing(S)
	 ideal S'
   SeeAlso
      toM2Ring
///


doc ///
   Key
      oppositeRing
      (oppositeRing,NCRing)
   Headline
      Creates the opposite ring of a noncommutative ring
   Usage                    
      Aop = oppositeRing A  
   Inputs
      A : NCRing	
   Outputs        
      Aop : NCRing 
   Description
      Text 
         Given an NCRing A, this creates an NCRing whose defining NCIdeal is generated by 
	 the "opposites" - elements whose noncommutative monomial terms have been reversed - 
	 of the generators of the defining NCIdeal of A. If the coefficient ring of A is a
	 Bergman ring, an NCGroebnerBasis is computed for Aop.
      Example
          R = QQ[q]/ideal{q^4+q^3+q^2+q+1}
          A = skewPolynomialRing(R,q,{x,y,z,w}) 
	  x*y == q*y*x
          Aop = oppositeRing A
	  y*x == q*x*y 
   SeeAlso
      skewPolynomialRing		
///


doc ///
   Key
      normalFormBergman
      (normalFormBergman,List,NCGroebnerBasis)
      (normalFormBergman,NCRingElement,NCGroebnerBasis)
   Headline
      Calls Bergman for a normal form calculation
   Usage
      nfList = normalFormBergman(L,Igb)
   Inputs
      L : NCRingElement
          or a @ TO List @ of NCRingElements
      Igb : NCGroebnerBasis
   Outputs
      nfList : List
   Description
      Text
         This method takes a list of NCRingElements and calls Bergman to 
	 reduce each element to normal form relative to the given NCGroebnerBasis
	 for the NCIdeal defining the NCRing to which the list elements belong. 
      Example
         A = QQ{x,y,z}
	 f = y*z + z*y - x^2
	 g = x*z + z*x - y^2
	 h = z^2 - x*y - y*x
	 I = ncIdeal {f,g,h}
	 Igb = ncGroebnerBasis I
	 normalFormBergman(z^17,Igb)
///

-*
doc ///
   Key
      isReduced
   Headline
      Determines if a given element is in normal form with respect to a Groebner basis
   Usage
      b = w.isReduced
   Inputs
      w : NCRingElement
   Outputs
      b : Boolean
   Description
      Text
         This flag is set to true if a given element of an NCRing (considered 
	 as an element of an appropriate NCPolynomialRing) is in normal form 
         relative to a Groebner basis.
      Example
         A = QQ{x,y}
	 I = ncIdeal{y^2-x^2}
	 Igb = ncGroebnerBasis(I)
	 w = y^3+x*y^2
	 w.isReduced
	 w'=w % Igb
	 w'.isReduced
///
*-

doc ///
   Key
      hilbertBergman
      (hilbertBergman, NCQuotientRing)
      [hilbertBergman,DegreeLimit]
   Headline
      Calls Bergman to compute the Hilbert series of an NCQuotientRing
   Usage
     hseries = hilbertBergman(A)
   Inputs
     A : NCQuotientRing
     DegreeLimit => ZZ
   Outputs
     hseries : RingElement
   Description
      Text
         This method calls the Bergman function ncpbhgroebner to compute the Hilbert
	 series of an NCQuotientRing. The input ring must be a ring over QQ or ZZ/p.
	 At this time, the output is correct only for NCRings with a standard grading -
	 all generators have degree 1. The output is returned as a polynomial in ZZ[T].
      Example
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 hilbertBergman(B,DegreeLimit=>12)
///

doc ///
   Key
      (hilbertSeries, NCRing)
   Headline
      Computes the Hilbert series of an NCRing
   Usage
     hseries = hilbertSeries(A)
   Inputs
     A : NCRing
     Order => ZZ
   Outputs
     hseries : RingElement
   Description
      Text
         This method computes the Hilbert series of a graded NCRing.  If the ring is defined ober QQ or ZZ/p,
	 and standard graded, it calls @ TO hilbertBergman @.  Otherwise, if the ring is defined
	 over a field (and potentially not standard graded), then a basis is computed and the
	 generating function of the degrees of that basis is returned.  The degree to which one computes
	 the Hilbert series is controlled with the Order option.
         The output is returned as a polynomial in ZZ[T].
      Example
	 A = QQ{x,y,z}
	 hilbertSeries(A,Order=>10)
	 setWeights(A,{1,2,3})
	 hilbertSeries(A,Order=>10)
	 B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
	 hilbertSeries(B,Order=>10)
///

doc ///
   Key
      "Basic operations on noncommutative algebras"
   Description
      Text 
         The NCAlgebra package contains a number of methods for studying noncommutative
	 rings - primarily graded rings. The following three extended examples 
	 highlight the capabilities of the package. For a detailed account of the
	 Groebner basis calculations underlying nearly all of these methods, see
	 @ TO "Using the Bergman interface" @.
      Text
         Our first example concerns a three-dimensional Sklyanin algebra. This example is
	 a PI-ring. We define the ring as a quotient of the tensor algebra on three
	 generators by the two-sided ideal generated by the three elements listed.
      Example
         A = QQ{x,y,z}
      Text
         Users familiar with Macaulay2 will recognize the notation for a ring of
	 noncommutative polynomials is identical to that for local rings, see
	 @ TO (symbol SPACE, Ring, List) @. Thus the
	 previous line does not generate an error if you forget to load the NCAlgebra package. 
      Example
	 f = y*z + z*y - x^2
	 g = x*z + z*x - y^2
	 h = z^2 - x*y - y*x
	 B = A/ncIdeal{f,g,h}
      Text
         It is known that this algebra has a unique (up to rescaling) central element 
	 of degree 3. We can verify this claim computationally using @ TO centralElements @
	 and check that the element is regular to a given degree. See @ TO isLeftRegular @.
      Example
         centralElements(B,3)
	 j = y^3+x*y*z-y*x*z-x^3
	 isCentral j
	 apply(5,i->isLeftRegular(j,i+1))
      Text
         In fact, we can see that j is (up to scaling) the only normal element of degree 3.
	 See the discussion above for interpreting the output of @ TO normalElements @.
      Example
         normalElements(B,3,n,o)
	 basis(3,B)
      Text
         Recently, we studied noncommutative matrix factorizations over noncommutative
	 hypersurfaces. Here is a simple example. The hypersurface is B/(j). However,
	 iterated quotients are not yet implemented, so we define this ring as a 
	 quotient of the tensor algebra. Note the use of "promote" to ensure j is 
	 thought of as an element of the tensor algebra.
      Example
         use A
	 I = B.ideal
         J = ncIdeal promote(j,A)
	 B' = A/(I+J) 
      Text
         As in the commutative case, any minimal free resolution of a finitely generated
	 module over a noncommutative hypersurface is eventually given by a matrix 
	 factorization. We resolve the trivial module for B' by expressing it as the 
	 cokernel of a homogeneous matrix. 
      Example     
	 k = ncMatrix {gens B'}
	 M = rightKernelBergman rightKernelBergman k
	 N = rightKernelBergman M
      Text
         As discussed in @ TO "Using the Bergman interface" @, the method 
	 @ TO rightKernelBergman @ only computes the kernel of a module map to a 
	 certain homogeneous degree. Applying a theorem of Cassidy and Shelton, we
	 can be sure the matrices in any minimal graded free resolution of the trivial 
	 module of B' will have entries of homogeneous degree at most 3.	 
	 Thus we conclude M is the third syzygy 
	 module and N is the fourth. If we lift these matrices we see that M and N are
	 nearly a factorization of the central element j.
      Example       
	 BprimeToB = ncMap(B,B',gens B)
     	 liftM = BprimeToB M
	 liftN = BprimeToB N
	 liftM*liftN
      Text
         It appears that a change of basis will produce a matrix factorization. In general,
	 Bergman returns a generating set for the kernel, but it need not have any
	 nice properties. Here the NCAlgebra package can help by factoring a map. We 
	 would like M*M' = M'*M = j*I where I is the identity matrix.
      Example  	
	 jId = promote(j,B)*(ncMatrix applyTable(entries id_(ZZ^4), i -> promote(i,B)))
	 assignDegrees(jId,{2,2,2,3},{5,5,5,6});
      Text 
         The matrix jId is diagonal, and we have assigned degrees to make it compatible
	 with M. Now we factor the lift of M through jId.
      Example
	 M' = jId // liftM
         N
      Text
         We see that M' and N describe the same submodule and we check that the 
	 factorization worked:
      Example         
	 liftM*M' 
	 M'*liftM 
      Text
         The user can create noncommutative rings in ways other than specifying a
	 presentation. For our second example, consider a skew polynomial ring on four
	 generators, where generators skew-commute (but are not nilpotent). See
	 @ TO skewPolynomialRing @ for more details.
      Example
         C = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z,w})
      Text
         Let us briefly note that the user can also define a skew polynomial ring 
	 with coefficients in a commutative ring.
      Example
         R = QQ[q]/ideal{q^4+q^3+q^2+q+1}
	 B = skewPolynomialRing(R,q,{x,y,z,w})
	 x*y == q*y*x         
      Text
         Returning to the main example, we can define a graded Ore extension of C 
	 by specifying an automorphism.
	 The function @ TO ncMap @ is used to define a ring map. Note that ring maps 
	 are linear and multiplicative by definition but are not assumed to be  well-defined. 
      Example
         use C
         sigma = ncMap(C,C,{y,z,w,x})
	 isWellDefined sigma
      Text
         We form the Ore extension of C by sigma. See @ TO oreExtension @.
      Example         
         D = oreExtension(C,sigma,a)
      Text
         The new generator a is normal and regular in D. Regularity (on the left or right)
	 can be checked one homogeneous degree at a time. See @ TO isLeftRegular @. 
	 Thus a determines a graded automorphism f:D->D via a*r=f(r)*a.
      Example
         isNormal a
	 apply(5,i-> isLeftRegular(a,i+1))
         sigmaD = normalAutomorphism a
      Text
	 Given an automorphism, one can check to see which elements it normalizes
	 in any given degree.
      Example       
         normalElements(sigmaD,1)
	 normalElements(sigmaD,2)
      Text
         One can check for the presence of normal elements more generally. In our
	 example, since a is normal, a^2 will also be normal. It is the only normal
	 monomial of degree 2. A complete description of the normal elements in a
	 given degree is given by @ TO normalElements @. 
      Example
         normalElements(D,2,P,Q)
      Text
         Each component of the "normal variety" is a set of polynomial equations which must
	 be satisfied by the coefficients of the monomial basis for an element expressed
	 in that basis to be normal. In this case, the basis of D in degree 2 is
      Example
         basis(2,D)	 
      Text
         The output of normalElements tells us that in order for a degree 2 element of D
	 to be normal, it must be an expresison in powers of the generators. The coefficients
	 of these powers must satisfy the six equations listed.
      Example
         isNormal (x^2+z^2-y^2-w^2)	 
      Text
         In Macaulay2, the user can define a polynomial ring to be commutative or 
	 skew-commutative (the exterior algebra). The user can convert these rings (and
	 their quotients) to a type compatible with the NCAlgebra package using 
         @ TO toNCRing @.
      Example
         E' = QQ[x,y,z,w,SkewCommutative=>true]
	 E = toNCRing E'
	 f = ncMap(E,C,gens E)
	 f x^2       
	 use C
	 x^2 == 0
      Text
         Conversely, the user can convert an NCRing to a (quotient of a) polynomial ring
	 in the usual sense of Macaulay2 using @ TO toM2Ring @. This method works on any
	 NCRing - the result is the abelianization or "exterior-ization" of the given ring.
	 For example, if we abelianize the skew polynomial ring C, we get a ring in which
	 only powers of the generators are nonzero. On the other hand, if we "exterior-ize"
	 C, we get the exterior algebra.
      Example
         C' = toM2Ring C
         x*y 
	 x*x
	 C'' = toM2Ring(C,SkewCommutative=>true)
         y*x
	 y*y
      Text
         Finally, we can construct the opposite ring. The opposite ring of D will be the
	 Ore extension by the inverse of sigma. See @ TO oppositeRing @.
      Example
         Dop = oppositeRing D
         a*x == w*a            
	 use D
	 a*w == x*a

      Text
         Our last extended example illustrates how to obtain a presentation for the
	 endomorphism ring of a module over a commutative ring. First we define a
	 hypersurface ring and a high syzygy module.
      Example
         Q = QQ[a,b,c]
	 R = Q/ideal{a*b-c^2}
	 kRes = res(coker vars R, LengthLimit=>7);
	 M = coker kRes.dd_5
      Text
         The endomorphism ring is computed using @ TO endomorphismRing @. This method
	 computes a presentation, but the presentation is typically not minimal. We
	 see from the following calcuation that X_3 = X_0X_2.
      Example
         B = endomorphismRing(M,X); 
	 gensI = gens ideal B
      Text
         To eliminate redundant generators and relations, use @ TO minimizeRelations @. 
	 This method makes several passes through the presentation, and stops if no 
	 minimization occurs.
      Example
         gensIMin = minimizeRelations(gensI, Verbosity=>1)
      Text
         We see a substantial reduction in the number of relations and that X_1 and X_3
	 are redundant generators. The endomorphisms themselves are cached, and can be
	 accessed via @ TO endomorphismRingGens @. As an example, we explicitly verify 
	 that X_3 is redundant. 
      Example
         maps = B.cache.endomorphismRingGens
         maps#3 == maps#0*maps#2
///

doc ///
   Key
      "Using the Bergman interface"
   Description
      Text
         Bergman is a software package for computing Groebner bases 
	 for ideals in both commutative and noncommutative polynomial 
	 rings with coefficients in Q or Z/p. Bergman was created by 
	 J. Backelin (U. of Stockholm) and its capabilities were extended 
	 by V. Ufnarovski, S. Cojacaru, and A. Podoplelov.
      Text
         Though Bergman is limited in terms of the coefficients it can 
	 handle and the choice of orderings it offers, it is a very 
	 efficient (especially in terms of memory usage) open source
	 program for computing noncommutative Groebner bases. (In the future
	 we may add support for other Groebner basis software.) 
	 Rather than re-inventing this wheel, the
	 NCAlgebra package makes extensive use of Bergman calls for
	 noncommutative Groebner basis calculations. The following 
	 examples illustrate some common calculations which involve
	 a call to Bergman. Our goal is to provide a more intuitive 
	 user experience with minimal compromises to efficiency. 
      Text
         Typically, the user begins by defining a noncommutative polynomial
	 ring. By default, the ring is graded with generators in degree 1.
	 Other gradings can be defined, see @ TO setWeights @.
      Example
         A = QQ{x,y,z}
      Text
         Implicit in this definition is a choice of ordering for Groebner basis
	 calculations: the ordering is degree-lexicographic with the generator
	 symbols listed in order from smallest to largest. Ring elements are 
	 displayed with the high term listed first.
      Example
         p = y*z + z*y - x^2
      Text
         One can try to compute Groebner bases for both homogeneous and inhomogeneous ideals.
	 We cannot ensure Bergman computes any other than the homogeneous case correctly. 
	 We consider only homogeneous examples, the inhomogeneous case being similar.
      Example
         q = x*z + z*x - y^2
       	 r = z^2 - x*y - y*x
       	 I = ncIdeal {p,q,r}
      Text
         NCAlgebra has three methods pertaining to  noncommutative
	 Groebner bases. One is @ TO twoSidedNCGroebnerBasisBergman @. This command
	 runs Bergman to compute a noncommutative Groebner basis to a certain degree.
	 The user will recall that unlike the commutative case, noncommutative 
	 Groebner bases need not be finite, and may grow rapidly. For unfamiliar
	 examples, we recommend initially setting a relatively low degree threshold 
	 (say, n=5). In our example, we know from experience the Groebner basis is finite
	 so we do not specify a degree limit.
      Example
          Igb = twoSidedNCGroebnerBasisBergman I
      Text
          One an NCGroebner basis has been calculated, it is cached for later use. This
	  option can be disabled. See @ TO twoSidedNCGroebnerBasisBergman @ for more
	  on options. We note that twoSidedNCGroebnerBasisBergman is called automatically
	  any time the user attempts to create an @ TO NCQuotientRing @.
      Text
          Another method related to Groebner bases is @ TO ncGroebnerBasis @. This 
	  method creates the @ TO NCGroebnerBasis @ object. By default, this method
	  also calls Bergman for a noncommutative Groebner basis calculation. However,
	  by setting the option InstallGB to true, the user instructs Macaulay2 to bypass 
          the Bergman call and accept the input list as a Groebner basis without checking
	  it is one. This can be useful when the coefficient ring is not @ TO QQ @ or a finite
	  prime field.
      Example
         Igb2 = ncGroebnerBasis(I,InstallGB=>true) 
      Text
         As mentioned above, noncommutative Groebner bases can grow rapidly both in the
	 number and size of the terms. In some cases, it takes days to calculate a 
	 Groebner basis to the desired degree. So as not to repeat this calculation 
	 more than once, users might have a Groebner basis saved in a file. The method
	 @ TO gbFromOutputFile @ enables the user to load the Groebner basis from a file.
	 The file should contain nothing besides a list of noncommutative polynomials in
	 Macaulay2 readable form. (One exception: Bergman output files contain lines 
	 beginning with the "%" symbol. gbFromOutputFile ignores lines beginning with a
	 "%".) Bergman users: you need not alter the output file from ncpbhgroebner in any
         way prior to calling gbFromOutputFile. See @ TO gbFromOutputFile @ for an example.
      Text
         Once a Groebner basis is computed, many methods become available. The most
	 basic calculation is to return the normal form of a given element relative
	 to the known Groebner basis. The NCAlgebra package also provides multiple 
	 options for this calculation. 
      Text
         Generally speaking, Bergman is the most efficient way to reduce a ring 
	 element to normal form. Behind the scenes, the NCAlgebra package creates a
	 Bergman-readable input file, runs a Bergman session, and interprets the 
	 output, which it displays to the user. This takes time, especially when the
	 Groebner basis is large. The NCAlgebra package has its own normal form
	 reduction algorithm. It is considerably slower than Bergman, but it can
	 be faster than the time required to execute the Bergman call. 
      Text
         The file NCAlgebra.m2 contains two environment variables: MAXDEG and MAXSIZE.
	 If the element to be reduced has degree less than MAXDEG and fewer than MAXSIZE
	 terms (or if the coefficient ring is not Q or Z/p), the NCAlgebra package calls
	 its own normal form reduction method. Otherwise, it calls @ TO normalFormBergman @.
	 The user can force a Bergman call using this method.
      Example
          z^17 % Igb
          normalFormBergman(z^17,Igb)
      Text
         Normal form calculations are performed automatically in an @ TO NCQuotientRing @.
      Example
         B = A/I
	 z^17
      Text
         We also use Bergman to compute Hilbert series of an @ TO NCQuotientRing @ using 
	 the @ TO hilbertBergman @ command. By default, the Hilbert series is given to 
	 degree 10. As mentioned above, we suggest reducing the degree limit for rings
	 whose growth is not well understood beforehand.
      Example
         hilbertBergman B 
      Text
         Perhaps a major achievement of the NCAlgebra package is the method 
	 @ TO rightKernelBergman @. The functionality described above involves only
	 the most basic Bergman calls, and long time Bergman users may find little
	 reason to prefer NCAlgebra on those grounds. On the other hand, computing 
	 kernel generators for a matrix with entries in a noncommutative ring is 
	 anything but straightforward, and we reduce the call to a single command.
      Example
         B = threeDimSklyanin(QQ,{1,1,-1},{x,y,z})
         A = ambient B
	 g = -y^3-x*y*z+y*x*z+x^3
         C = A/(ideal B + ncIdeal g)
         M = ncMatrix {{x,y,z,0}, {-y*z-2*x^2,-y*x,z*x-x*z,x},{x*y-2*y*x,x*z,-x^2,y}, {-y^2-z*x,x^2,-x*y,z}}
      Text
         For details about matrices over noncommutative rings, see @ TO NCMatrix @.
	 Provided the entries of M are homogeneous and their degrees are compatible,
	 M can be viewed as a graded (degree 0) homomorphism of graded free B-modules.
	 For more on this degree-compatibility, see @ TO assignDegrees @.
      Example
         assignDegrees(M,{1,0,0,0},{2,2,2,1})
      Text
         Now, we can compute the kernel of M. It is always assumed that M determines a 
	 map of graded right modules. As always in the noncommutative case, computing
	 generators of the kernel of a map is generally an infinite linear algebra
	 problem. The @ TO rightKernelBergman @ method returns a set of minimal kernel
	 generators to degree 10, or to the degree specified by the user.
      Example
         ker1M = rightKernelBergman(M)
         M*ker1M == 0
      Text
         In principle, this method can be used to compute the minimal free resolution
	 of a finitely generated B-module with known presentation up to a specified degree.
      Example
         ker2M = rightKernelBergman(ker1M)
	 ker3M = rightKernelBergman(ker2M) 
   SeeAlso
      "General setup information"
///

doc ///
   Key
      "General setup information"
   Description
      Text
         We recommend using the NCAlgebra package with the most recent version of Macaulay2. 
      Text 
         Many of the methods in the NCAlgebra package rely on J. Backelin's
	 noncommutative Groebner basis program Bergman. In the future, we may
	 add support for other programs which compute Groebner bases, but for
	 now, users must have Bergman installed.
      Text
         Detailed instructions for installing Bergman, as well as the NCAlgebra system, can be
	 found in the file installNCAlgebra.txt file contained in the NCAlgebra package directory.
	 It may also be found at @HREF"https://github.com/Macaulay2/M2/blob/master/M2/Macaulay2/packages/NCAlgebra/installNCAlgebra.txt"@.
   SeeAlso
      "Using the Bergman interface"
///

doc ///
  Key
    kernelComponent
    (kernelComponent, ZZ, NCRingMap)
  Headline
    Computes a basis of the kernel of a ring map in a specified degree.
  Usage
    kern = kernelComponent(n,phi)
  Inputs
    n : ZZ
    phi : NCRingMap
  Outputs
    kern : NCMatrix
  Description
    Text
       This function returns a basis of the kernel of a ring map in a specified degree.
    Example
       A = QQ{x,y,z}
       B = skewPolynomialRing(QQ,(-1)_QQ, {a,b,c})
       phi = ncMap(B,A,{a,b,c})
       kernelComponent(2,phi)
///

doc ///
  Key
    gddKernel
    (gddKernel, ZZ, NCRingMap)
  Headline
    Computes a homogeneous generating set of the kernel of a ring map.
  Usage
    kern = gddKernel(n,phi)
  Inputs
    n : ZZ
    phi : NCRingMap
  Outputs
    kern : List
  Description
    Text
       This function returns a generating set of the kernel of a
       ring map up to a specified degree.
    Example
       A = QQ{x,y,z}
       B = skewPolynomialRing(QQ,(-1)_QQ, {a,b,c})
       phi = ncMap(B,A,{a,b,c})
       gddKernel(4,phi)
///

doc ///
  Key
    envelopingAlgebra
    (envelopingAlgebra, NCRing, Symbol)
  Headline
    Create the enveloping algebra
  Usage
    Ae = envelopingAlgebra(A,x)
  Inputs
    A : NCRing
    x : Symbol
  Outputs
    Ae : NCRing
  Description
    Text
       This function returns the enveloping algebra A ** Aop.  The symbol
       argument is used to define the variables in Aop.
    Example
       B = skewPolynomialRing(QQ,(-1)_QQ, {a,b,c})
       envelopingAlgebra(B,v)
///

doc ///
  Key
    freeProduct
    (freeProduct, NCRing, NCRing)
  Headline
    Define the free product of two algebras
  Usage
    C = freeProduct(A,B)
  Inputs
    A : NCRing
    B : NCRing
  Outputs
    C : NCRing
  Description
    Text
       This function returns the free product of the algebras A and B.
    Example
       A = QQ{x,y,z}
       B = skewPolynomialRing(QQ,(-1)_QQ, {a,b,c})
       C = freeProduct(A,B)
///

doc ///
  Key
    qTensorProduct
    (qTensorProduct,NCRing,NCRing,ZZ)
    (qTensorProduct,NCRing,NCRing,QQ)
    (qTensorProduct,NCRing,NCRing,RingElement)
    (symbol **, NCRing, NCRing)
  Headline
    Define the (q-)commuting tensor product
  Usage
    C = qTensorProduct(A,B,q)
  Inputs
    A : NCRing
    B : NCRing
    q : RingElement
  Outputs
    C : NCRing
  Description
    Text
       This function returns the algebra that contains A and
       B as a subalgebra, with the commutation law on the 
       images of A and B given by a*b = q*b*a for all a in A and b in B.
       In the case of A ** B, q = 1.
    Example
       A = QQ{x,y}
       B = skewPolynomialRing(QQ,(-1)_QQ, {a,b})
       C = qTensorProduct(A,B,-1_QQ)
       ideal C
       D = A ** B
       ideal D
///

doc ///
  Key
    (Hom,ZZ,NCMatrix,NCMatrix)
  Headline
    Compute a graded component of Hom(M,N)
  Usage
    homs = Hom(n,M,N)
  Inputs
    n : ZZ
    M : NCMatrix
    N : NCMatrix
  Outputs
    homs : List
  Description
    Text
      This function computes a generating set of the degree n component
      of Hom(M,N) as a module over the coefficient ring of A = ring M.
///

doc ///
  Key
     (symbol _, NCMatrix, ZZ)
  Headline
    Induced map in homogeneous degree n
  Usage
    m = M_n
  Inputs
    M : NCMatrix
    n : ZZ
  Outputs
    m : Matrix
  Description
    Text
      If we regard M as defining a map of graded right free modules,
      then M_n returns the induced map of underlying R-modules in degree
      n, where R is the coefficient ring of ring M.
///

doc ///
  Key
    (symbol SPACE, NCMatrix, Array)
  Headline
    Graded shift of an NCMatrix.
  Usage
    N = M x
  Inputs
    M : NCMatrix
    x : Array
  Outputs
    N : NCMatrix
  Description
    Text
      Shifts the source and target in degrees by n if x = [n].
///

doc ///
  Key
    (symbol +, NCRingMap, NCRingMap)
    (symbol *, ZZ, NCRingMap)
    (symbol *, QQ, NCRingMap)
    (symbol *, RingElement, NCRingMap)
    (symbol ^, NCRingMap, ZZ)
  Headline
    Basic operations with NCRingMaps
  Usage
    h = f + g or r*f
  Inputs
    f : NCRingMap
    g : NCRingMap
  Outputs
    h : NCRingMap
  Description
    Text
      Defines the sum of NCRingMaps.  Though a linear combination of
      ring maps is not a ring map in general, this routine is useful in
      constructing ring maps programmatically.  The sum is defined only on generators of
      the common source of f and g, while for higher degree monomials m,
      one no longer has f(m) + g(m) = h(m) (so it is only the sum on words
      of length 1).
    Example
      A = QQ{x,y}
      f = ncMap(A,A,{x,y})
      g = ncMap(A,A,{y,x})
      h = 3*f + 4*g
      matrix h
      k = h^3
      matrix k      
///

doc ///
  Key
    (symbol SPACE, NCRingMap, NCIdeal)
    (symbol SPACE, NCRingMap, NCGroebnerBasis)
  Headline
    Apply a ring map to the generators of an ideal
  Usage
    J = f I
  Inputs
    f : NCRingMap
    I : NCIdeal
        or @ TO NCGroebnerBasis @
  Outputs
    J : NCIdeal
        or @ TO NCGroebnerBasis @
  Description
    Text
      This function applies the ring map f to the generators of the ideal
      or noncommutative Groebner basis that is passed in.
    Example
      A = QQ{x,y}
      g = ncMap(A,A,{y,x})
      I = ncIdeal {x^2*y+y*x^2}
      g I
///

doc ///
  Key
    (resolution, NCMatrix)
    NCChainComplex
    (betti,NCChainComplex)
  Headline
    Compute the resolution of coker M as a map of free right modules
  Usage
    res M
  Inputs
    M : NCMatrix
    LengthLimit => ZZ 
       The length of the resolution.  This defaults to the number
       of variables of ring M.
  Outputs
    : NCChainComplex
  Description
    Text
      This function computes a minimal graded free resolution
      of the cokernel of M, considered as a map of graded right
      free modules.  M must be homogeneous for this command to work.
      
      As of this version, NCChainComplex (the return type) is still
      quite simple, though @ TO betti @ still works on them.
    Example
      A = skewPolynomialRing(QQ,(-1)_QQ,{x,y,z})
      M = ncMatrix {{x,y,z}}
      Mres = res M
      Mres#0
      Mres#1
      Mres#2
      betti Mres
  SeeAlso
    resolution
///

--warning: tag has no documentation: NCAlgebra :: resolution(NCMatrix), key (resolution,NCMatrix)
--warning: tag has no documentation: NCAlgebra :: ring(NCGroebnerBasis), key (ring,NCGroebnerBasis)
