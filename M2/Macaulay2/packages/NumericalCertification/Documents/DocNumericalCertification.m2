-- Documents for Numerical Certification.



-- These are methods/functions only used as a mechanism. (will not be used by users)
undocumented{(substitute, RingElement, CCiMatrix),
    sqabsForGaussianRational, (sqabsForGaussianRational, RingElement),
    conjugateGaussian, (conjugateGaussian, RingElement),
    pointNorm, -*(pointNorm, AbstractPoint),*- (pointNorm, Matrix),
    conjugateGaussianRationalMatrix, (conjugateGaussianRationalMatrix, Matrix)
--    subOnTerm, (subOnTerm, Number, Matrix), (subOnTerm, RingElement, Matrix),
--    (subOnTerm, RingElement, CCiMatrix)
    }


-- These are already documented as methods.

-- options for alphaCertified.
undocumented{ALGORITHM, ARITHMETICTYPE, PRECISION, REFINEDIGITS, NUMRANDOMSYSTEMS,
    RANDOMDIGITS, RANDOMSEED, NEWTONONLY, NUMITERATIONS, REALITYCHECK, REALITYTEST, toACertifiedPoly}



-- These interval arithmetic methods are documented in the documents of its classes.
undocumented{(symbol +, CCi, CCi), (symbol *, CCi, CCi),
    (symbol *, CCi, Number), (symbol +, CCi, Number),
    (symbol -, CCi, Number),
    (symbol *, Number, CCi), (symbol +, Number, CCi),
    (symbol -, Number, CCi),
    (symbol ^, CCi, Number), (symbol /, CCi, CCi),
    (symbol +, CCiMatrix, CCiMatrix), (symbol *, CCiMatrix, CCiMatrix),
    (symbol *, CCiMatrix, Number), (symbol +, CCiMatrix, Matrix),(symbol *, Matrix,CCiMatrix),
    (symbol -, CCiMatrix, Matrix),(symbol -, CCiMatrix, CCiMatrix),
    (symbol *, Number, CCiMatrix), (symbol +, Matrix, CCiMatrix),
    (symbol -, Matrix, CCiMatrix), (matrixCCiApply, CCiMatrix, FunctionClosure),
    (midpointMatrix, CCiMatrix), (net, CCiMatrix), (subFloat, RingElement, CCiMatrix),
    (subOnTerm, RingElement, CCiMatrix), (subOnTermCC, RingElement, CCiMatrix),
    (symbol ^, CCiMatrix, Number),
    (flatten, CCi), (isEmpty, CCi), (net, CCi)}

undocumented{(intervalCCi,Number), (intervalCCi,Number,Number),
    (intervalCCi,Number,RRi),(intervalCCi,RRi),(intervalCCi,RRi,Number),(intervalCCi,RRi,RRi),
    (matrixCCi, AbstractPoint), (matrixCCi, List), (matrixCCi,Matrix, Matrix)}

undocumented{(certifyRegularSolution,PolySystem,AbstractPoint,Sequence),
    (certifyRegularSolution,PolySystem,Matrix,Sequence),
    (certifyRealSolution,PolySystem,Matrix,Sequence),
    (certifyRealSolution,PolySystem,AbstractPoint,Sequence),
    (certifyDistinctSolutions,PolySystem,List,List)}


doc ///
    	Key
	    	NumericalCertification
	Headline
	    	certify a numerical solution for a square system
	Description
	    	Text
		    	This package provides symbolic-numeric approaches to certify roots for a square polynomial system.

			For regular roots, the package has two different approaches.
		      	The first is Smale's alpha theory and the second is Krawczyk method via interval arithmetic.
			Both methods are based on Newton's method and they all describe the specific region containing a unique root of the system.

			In the case of alpha theory, this package follows the algorithms of alpha theory established in @HREF("https://arxiv.org/abs/1011.1091","\"alphaCertified: certifying solutions to polynomial systems\" (2012)")@.
			In the case of Krawczyk method, this package follows the theory introduced in @HREF("https://epubs.siam.org/doi/book/10.1137/1.9780898717716","\"Introduction to Interval Analysis\" (2009)")@.
			These two methods also support not only floating-point arithmetic over the real and complex numbers, but also the exact computation with inputs of rational numbers.

			Moreover, the package has a function certifying regular roots via a software @HREF("https://www.math.tamu.edu/~sottile/research/stories/alphaCertified/", "\"alphaCertified\"")@.

			For singular roots, the concept of the iterated deflation established in @HREF("https://www.sciencedirect.com/science/article/pii/S030439750600168X","\"Newton's method with deflation for isolated singularities of polynomial systems\" (2006)")@ is implemented.
			For a given system and a point, the package provides a function constructing a system having a given point as a regular solution and certifies it via alpha theory or interval arithmetic.

    	    	Text
		    	{\bf Certifying a list of solutions :}

			    $\bullet$ @TO "certifySolutions"@

			For a direct use of the package, a given polynomial system and a list of numerical solutions can be given.

		Example
		        R = CC[x1,x2,y1,y2];
		    	f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -3.5, x1^2 + y1^2 -1, x2^2 + y2^2 -1};
		    	sols = solveSystem f
			c = certifySolutions(f, sols, Strategy => "alphaTheory");
			peek c

    	    	Text
		    	For possible options for certification, see @TO "CertificationOptions"@.


		Text
		    	{\bf Regular Root Ceritification Methods:}

			    $\bullet$ @TO "certifyRegularSolution"@

			    $\bullet$ @TO "krawczykTest"@

			{\bf Examples}

		            The following example shows how to certify the roots of solutions for the square polynomial system.
			    This example is suggested in @HREF("https://www3.nd.edu/~jhauenst/preprints/hlPolyExp.pdf",
				"\"Certifying solutions to square systems of polynomial-exponential equations\" (2017)")@


			{\bf    $\bullet$ alpha theory}
		Text
		    A set of points for certification should be given in advance using other system solvers.
		Example
		    R = RR[x1,x2,y1,y2];
		    f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -3.5, x1^2 + y1^2 -1, x2^2 + y2^2 -1};
		    p1 = point{{.95, .32, -.30, .95}};
		    p2 = point{{.9, .3, -.3, 1}}; -- poorly approximated solution
		Text
		    It shows the results of the certification.
		Example
		    certifyRegularSolution(f,p1)
		    certifyRegularSolution(f,p2) -- not an approximate solution

		Text
		    Also, if we have other solutions of the system, alpha theory suggests an algorithm for distinguishing these solutions.
		Example
		    p1 = point{{.95,.32,-.30,.95}};
		    p2 = point{{.65,.77,.76,-.64}};
		    certifyDistinctSolutions(f,p1,p2)
		Text
		    In the case of real polynomial system, we can certify that a given solution is real or not.
		Example
		    p = point{{.954379, .318431, -.298633, .947949}};
		    certifyRealSolution(f,p)
		Text
		    Even more, when the polynomial and a point are given exact numbers over the rational number, the package computes auxiliary quantities exactly and performs an exact certification.
		Example
		    R = QQ[x1,x2,y1,y2]
		    f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2, x1^2 + y1^2 -1, x2^2 + y2^2 -1};
		    p = point{{95/100,32/100,-30/100,95/100}}; -- an input over the rational numbers
		    computeConstants(f,p)
		    certifyRegularSolution(f,p)

		Text
		    {\bf    $\bullet$ Krawczyk method}
		Text
		    Intervals for certification should be given in advance using other system solvers.
		Example
		    R = RR[x1,x2,y1,y2];
		    f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -3.5, x1^2 + y1^2 -1, x2^2 + y2^2 -1};
		    (I1, I2, I3, I4) = (interval(.94,.96), interval(.31,.33), interval(-.31,-.29), interval(.94,.96));
		Text
		    We set the relationships between variables and intervals using the matrix aligning entries in the order of variables of the polynomial ring.
		Example
    	    	    krawczykOperator(f,matrix{{I1,I2,I3,I4}})
		Text
		    The function @TO "krawczykTest"@ automatically checks whether the Krawczyk operator is contained in the input interval box.
		Example
		    krawczykTest(f,matrix{{I1,I2,I3,I4}})
		Text
		    For a given point, the function @TO "pointToInterval"@ provides a proper complex interval box for the input.
		    For constructing a complex interval and a matrix with complex interval entries, see @TO "CCi"@ and @TO "CCiMatrix"@.
		Example
		    R = RR[x1,x2,y1,y2];
		    f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -3.5, x1^2 + y1^2 -1, x2^2 + y2^2 -1};
		    p = point{{.954379, .318431, -.298633, .947949}};
		    I = pointToInterval(f,p)
		    krawczykTest(f,I)


		Text
		    	{\bf Multiple Root Ceritification Method:}

			    $\bullet$ @TO "certifySingularSolution"@

                        It is known that an isolated singular solution is regularized within finitely many steps by the iterated first order deflation (e.g. see  @HREF("https://www.sciencedirect.com/science/article/pii/S030439750600168X","\"Newton's method with deflation for isolated singularities of polynomial systems\" (2006)")@).
	        	The function @TO "certifySingularSolution"@ determines if a given point is associated to a singular solution of a given system using the deflation method.
	    	Example
    	    	    R = CC[x,y,z];
		    f = polySystem {x^2+y+z-1,x+y^2+z-1,x+y+z^2-1};
		    p = point{{1e-7-1e-7*ii,1e-7+1e-7*ii,1+1e-7}};
    	            certifySingularSolution(f,p)


///





doc ///
    	Key
    	    computeConstants
	    (computeConstants, PolySystem, AbstractPoint)
	    (computeConstants, PolySystem, Matrix)
	    "(computeConstants, PolySystem, AbstractPoint)"
	    "(computeConstants, PolySystem, Matrix)"
	Headline
	    compute the square of the auxiliary quantities related to alpha theory
	Usage
	    (alpha, beta, gamma) = computeConstants(PS, P)
	Inputs
            PS:PolySystem
	    P:AbstractPoint
	Description
	    Text
    	    	alpha theory uses three auxiliary quantities related to the input polynomial system and point.

		Beta value is defined by the length of the Newton step and gamma value is the quantity which is inversely proportional to the length between exact solution and the given point.

		Alpha value is defined by the multiplication of beta and gamma. When it is smaller than  $0.157671$, then the input point is an approximate solution to the system. The function @TO "certifyRegularSolution"@ does this process.
	    Example
	        R = RR[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -3.5,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
		p = point{{.95,.32,-.30,.95}};
    	    	(a, b, g) = computeConstants(f,p)
///



doc ///
    	Key
    	    certifyRegularSolution
	    (certifyRegularSolution, PolySystem, AbstractPoint)
	    (certifyRegularSolution, PolySystem, Matrix)
	    "(certifyRegularSolution, PolySystem, AbstractPoint)"
	    "(certifyRegularSolution, PolySystem, Matrix)"
	Headline
	    certify whether a given point is an approximate solution to the system
	Usage
	    alpha = certifyRegularSolution(PS, P)
	Inputs
            PS:PolySystem
	    P:AbstractPoint
	Description
	    Text
    	    	This function executes the alpha test based on the value computed by @TO "computeConstants"@.
	    Example
	        R = RR[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -3.5,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
	    Text
	    	Input can be a @TO "AbstractPoint"@ or @TO "Matrix"@ representing coordinates or a list of points or matrices.
	    Example
		p1 = point{{.95,.32,-.30,.95}};
    	    	certifyRegularSolution(f,p1)
///



doc ///
    	Key
    	    certifyDistinctSolutions
	    (certifyDistinctSolutions, PolySystem, AbstractPoint, AbstractPoint)
	    (certifyDistinctSolutions, PolySystem, Matrix, Matrix)
	    "(certifyDistinctSolutions, PolySystem, AbstractPoint, AbstractPoint)"
	    "(certifyDistinctSolutions, PolySystem, Matrix, Matrix)"
	Headline
	    determine whether given points are distinct approximate solutions to the system
	Usage
	    certifyDistinctSolutions(PS, P1, P2)
	Inputs
            PS:PolySystem
	    P1:AbstractPoint
	    P2:AbstractPoint
	Description
	    Text
    	    	This function executes the gamma test based on the value computed by @TO "computeConstants"@, and determine whether given points are distinct or not.
	    Example
	        R = RR[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -3.5,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
		p1 = point{{.95,.32,-.30,.95}};
		p2 = point{{.65,.77,.76,-.64}};
    	    	certifyDistinctSolutions(f,p1,p2)
	    Text
	    	However, if two solutions are too close, it concludes that inputs are not distinct.
	    Example
		p1 = point{{.6525,.7712,.7577,-.6366}};
		p2 = point{{.653,.771,.758,-.637}};
    	    	certifyDistinctSolutions(f,p1,p2)
	    Text
	    	Even worse, if two solutions are close enough and both have alpha value which are bigger than $0.03$, it gives indecisive comments.

		In this case, user should apply @TO "newton"@ to the point to get more precise approximation.
	    Example
		p1 = point{{.95,.32,-.30,.95}};
		p2 = point{{.95,.32,-.301,.95}};
    	    	certifyDistinctSolutions(f,p1,p2)
///



doc ///
    	Key
    	    certifyRealSolution
	    (certifyRealSolution, PolySystem, AbstractPoint)
	    (certifyRealSolution, PolySystem, Matrix)
	    "(certifyRealSolution, PolySystem, AbstractPoint)"
	    "(certifyRealSolution, PolySystem, Matrix)"
	Headline
	    determine whether a given point is an real approximate solution to the system
	Usage
	    certifyRealSolution(PS, P)
	Inputs
            PS:PolySystem
	    P:AbstractPoint
	Description
	    Text
    	    	When the system is real (or rational) polynomial system, this function executes the gamma test based on the value computed by @TO "computeConstants"@, and determine whether a given point is a real approximate solution  or not.
	    Example
	        R = RR[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -3.5,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
		p = point{{.954379,.318431,-.298633,.947949}};
    	    	certifyRealSolution(f,p)
	    Text
	    	However, an input point is poorly approximated, it gives false even if the point is real.
		In this case, user should apply @TO "newton"@ to the point to get more precise approximation.
	    Example
		p = point{{.65,.77,.75,-.64}};  -- poorly approximated solution
    	    	certifyRealSolution(f,p)
///



doc ///
    	Key
    	    alphaTheoryCertification
	    (alphaTheoryCertification, PolySystem, List)
	    "(alphaTheoryCertification, PolySystem, List)"
	Headline
    	    executes alpha-certification on a given system and list of points
	Usage
	    (D, R, CS, C) = alphaTheoryCertification(PS, L)
	Inputs
            PS:PolySystem
	    L:List
	Outputs
	    D:List
	    	a list of certified distinct solutions
	    R:List
	    	a list of certified real solutions
	    CS:List
	    	a list of certified solutions
	    C:List
	    	a list of constants for certified solutions
	Description
	    Text
	        This function does all procedures of @TO "certifyRegularSolution"@, @TO "certifyDistinctSolutions"@ and @TO "certifyRealSolution"@ at once.
	    Example
	        R = RR[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -3.5,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
		p1 = point{{.954379,.318431,-.298633,.947949}}; p2 = point{{.95, .32, -.30, .95}}; p3 = point{{.652567, .77115, .757776, -.636663}}; p4 = point{{.65, .77, .76, -.64}};
		p5 = point{{.31, .30, .72, -.60}}; -- poorly approximated solution
		P = {p1, p2, p3, p4, p5}
    	        alphaTheoryCertification(f,P)
///


doc ///
    	Key
	    krawczykOperator
	    (krawczykOperator, PolySystem, Matrix)
	    (krawczykOperator, PolySystem, CCiMatrix)
	    (krawczykOperator, PolySystem, AbstractPoint)
	    (krawczykOperator, Matrix, Matrix)
	    (krawczykOperator, Matrix, CCiMatrix)
	    (krawczykOperator, Matrix, AbstractPoint)
	Headline
	    compute the Krawczyk operator
	Description
	    Text
	    	For given interval and polynomial system, this function computes the Krawczyk operator.
	    Example
	    	R = RR[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
	        (I1, I2, I3, I4) = (interval(.94,.96), interval(.31,.33), interval(-.31,-.29), interval(.94,.96));
	    Text
	        Intervals for certification should be given as a Matrix, and we set the relationships between variables and intervals by aligning them in the order of variables of the polynomial ring.
	        For constructing a proper interval box from a given point, see the function @TO "pointToInterval"@.
	    Example
		M = matrix{{I1,I2,I3,I4}}
    	    Text
	    	If the Krawczyk operator is contained in the input interval, then we conclude that the input interval (or the Krawczyk operator) contains a unique root of the system.
	    Example
    	    	krawczykOperator(f,M)
    	    Text
		The function @TO "krawczykTest"@ checks this criterion automatically.

///


doc ///
    	Key
	    krawczykTest
	    (krawczykTest, PolySystem, Matrix)
	    (krawczykTest, PolySystem, CCiMatrix)
	    (krawczykTest, PolySystem, AbstractPoint)
	    (krawczykTest, PolySystem, List)
	    (krawczykTest, Matrix, Matrix)
	    (krawczykTest, Matrix, CCiMatrix)
	    (krawczykTest, Matrix, AbstractPoint)
	    (krawczykTest, Matrix, List)
	Headline
	    certify the interval box for square polynomial system
	Description
	    Text
	    	For given interval and polynomial system, this function computes the Krawczyk operator and check that the operator is contained in the input interval.
	    Example
	    	R = RR[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
	        (I1, I2, I3, I4) = (interval(.94,.96), interval(.31,.33), interval(-.31,-.29), interval(.94,.96));
	    Text
	        Intervals for certification should be given as a Matrix, and we set the relationships between variables and intervals by aligning them in the order of variables of the polynomial ring.
	        For constructing a proper interval box from a given point, see the function @TO "pointToInterval"@.
	    Example
		M = matrix{{I1,I2,I3,I4}}
	    Text
	    	If the Krawczyk operator is contained in the input interval, then the function returns the result that the input interval (or the Krawczyk operator) contains a unique root of the system.
	    Example
    	    	krawczykTest(f,M)
	    Text
	    	If the function encounters a @TO "AbstractPoint"@ as an input, then it computes a proper interval box for the given point using @TO "pointToInterval"@ function.
	    Example
		p = point {{.95437+0.0001*ii, .318445, -.298627, .947941}}
    	    	krawczykTest(f,p)



///


doc ///
    	Key
	    krawczykRealnessTest
	    (krawczykRealnessTest, PolySystem, CCiMatrix)
	    (krawczykRealnessTest, PolySystem, AbstractPoint)
	    (krawczykRealnessTest, PolySystem, List)
	    (krawczykRealnessTest, Matrix, CCiMatrix)
	    (krawczykRealnessTest, Matrix, AbstractPoint)
	    (krawczykRealnessTest, Matrix, List)
	Headline
	    certify the realness of the associated solution for the square polynomial system from the given interval box
	Description
	    Text
	    	For given interval and polynomial system, this function determines if the associated solution is real or not.
	    Example
	    	R = CC[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
		p = point {{.95437+0.0001*ii, .318445, -.298627, .947941}} -- a numerical solution over the complex number
    	    	I = pointToInterval(p, 1e-2) -- an interval box centered at p with radius 1e-2
	    Text
	        Intervals for certification should be given as a Matrix, and we set the relationships between variables and intervals by aligning them in the order of variables of the polynomial ring.
	        For constructing a proper interval box from a given point, see the function @TO "pointToInterval"@.
	    	If the given interval box passes the Krawczyk test and its associated solution is real, then the function returns true
	    Example
    	    	krawczykRealnessTest(f,I)



///




doc ///
    	Key
    	    certifySingularSolution
	    (certifySingularSolution, PolySystem, AbstractPoint)
	    (certifySingularSolution, PolySystem, AbstractPoint, Number)
	    (certifySingularSolution, PolySystem, Matrix)
	    (certifySingularSolution, PolySystem, Matrix, Number)
	    (certifySingularSolution, PolySystem, CCiMatrix)
	    (certifySingularSolution, PolySystem, CCiMatrix, Number)
	Headline
    	    certify if a given point is a singular solution for a given system using the deflation method.
	Description
	    Text
	        This function determines if a given point or interval box is associated to a singular solution for a given system.
		It uses the fact that a singular solution can be regularized via the deflation method (e.g. see  @HREF("https://www.sciencedirect.com/science/article/pii/S030439750600168X","\"Newton's method with deflation for isolated singularities of polynomial systems\" (2006)")@.
		{\bf Caveat :} Certification is done by a subsystem obtained from the deflation method. It may produce a false positive result probabilistically.
	    Example
    	    	    R = CC[x,y,z];
		    f = polySystem {x^2+y+z-1,x+y^2+z-1,x+y+z^2-1};
		    p = point{{1e-7-1e-7*ii,1e-7+1e-7*ii,1+1e-7}};
    	            certifySingularSolution(f,p)
	    Text
                It is known that an isolated singular solution is regularized within finitely many steps by the iterated first order deflation (e.g. see  @HREF("https://www.sciencedirect.com/science/article/pii/S030439750600168X","\"Newton's method with deflation for isolated singularities of polynomial systems\" (2006)")@).
		A positive integer can be given as a number of iterations. If no number is given, it iterates until the solution is regularized (hence, it may not be terminated).
	    Example
	        R = CC[x,y]
		f = polySystem {x+y^3,x^2*y-y^4};
		p = point {{-3.38813e-21+1.35525e-20*ii, -3.38813e-21+2.03288e-20*ii}};
    	        certifySingularSolution(f,p,2) -- false, two iterations are not enough
		certifySingularSolution(f,p,3)
	    Text
	        As it checks the regularity of a numerical point eventually, two strategies (alpha theory and interval arithmetic) can be used.
	    Example
    	    	certifySingularSolution(f,p,3,Strategy => "intervalArithmetic")
///


doc ///
    	Key
	    CertificationOptions
	    Strategy
	    [certifySingularSolution, Strategy]
	    [certifySolutions, Strategy]
	    "alphaTheory"
	    "intervalArithmetic"
      	Headline
	    options for certification method
	Description
	    Text
	    	This is an option for @TO "certifySingularSolution"@ and @TO "certifySolutions"@. There are three possible options {\tt alphaTheory, intervalArithmetic} and {\tt alphaCertified}. By default, it takes alpha theory as a strategy.
	    Example
	    	R = RR[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
		p = point {{.95437, .318445, -.298627, .947941}};
    	    	peek certifySolutions(f, {p})
		peek certifySolutions(f, {p}, Strategy => "intervalArithmetic")
///



doc ///
    	Key
    	    certifySolutions
	    (certifySolutions, PolySystem, List)
	    (certifySolutions, PolySystem, List, Number)
	    "(certifySolutions, PolySystem, List)"
	Headline
    	    executes certification on a given system and list of points
	Usage
	    certifySolutions(PS, P)
	Inputs
            PS:PolySystem
	    L:List
	Outputs
	    H:MutableHashTable
	    	a MutableHastTable contains data of a list of given solutions
	Description
	    Text
	        This function does all procedures for root certification. It takes three strategies as options which are ''alphaTheory'', ''intervalArithmetic'' and ''alphaCertified'' (see @TO "CertificationOptions"@).
		The option ''alphaTheory'' returns alpha values of solutions, a list of regular distinct solutions, a list of real solutions, a list of regular solutions, a list of certified singular solutions and a list of non-certified solutions
	    Example
    	    	R = CC[x,y,z];
	    	f = polySystem {(x-y)^3 - z^2, (z-x)^3 - y^2, (y-z)^3 - x^2};
		listOfSols = solveSystem f; 
    	        c = certifySolutions(f,listOfSols);
		peek c
	    Text
		The option ''intervalArithmetic'' returns a list of (refined) regular real solutions, a list of (refined) regular solutions, a list of certified singular solutions and a list of non-certified solutions
	    Example
    	        c = certifySolutions(f,listOfSols,Strategy => "intervalArithmetic");
		peek c
	    Text
		The option ''alphaCertified'' can be used by {\tt certifySolutions(f,listOfSols,Strategy => "alphaCertified")}. It generates output files for using the software alphaCertified. Singular solutions may not be certified.

///








doc ///
    	Key
    	    pointToInterval
	    (pointToInterval, AbstractPoint, Number)
    	    (pointToInterval, PolySystem, AbstractPoint)
	Headline
	    finds an interval box from a given point
	Description
	    Text
	        This function finds an interval box from a given point. There are two ways to find an interval box. If a user has a desired radius for the interval box, then running the function with the point and the radius returns an interval centered at the point with the given radius.
	    Example
    	    	p = point{{.151879*ii, -.142332-.358782*ii, .142332-.358782*ii}};
    	    	I = pointToInterval(p, 1e-3) -- returns an interval box centered at the point with the radius 1e-3
	    Text
	        If a user doesn't know what radius to choose, but there is a given system, the function computes a proper interval box with the radius estimating the distance between the input point and its Newton convergence limit via the epsilon-inflation method (see @HREF("https://www.degruyter.com/document/doi/10.1515/9783110499469/html?lang=en", "\"Interval Analysis\"")@).
	    Example
	    	R = CC[x,y,z];
	    	f = polySystem {(x-y)^3 - z^2, (z-x)^3 - y^2, (y-z)^3 - x^2};
    	    	p = point{{.151879*ii, -.142332-.358782*ii, .142332-.358782*ii}};
    	    	I = pointToInterval(f,p)
///



doc ///
    	Key
    	    CCi
	    "intervalCCi"
	    "midpointCCi"
	    "intervalCCi(CCi)"
	    "intersect(CCi,CCi)"
	    "intersect(CCi,RRi)"
	    "intersect(RRi,CCi)"
	    "midpointCCi(CCi)"
	    "norm(CCi)"
	    "realPart(CCi)"
	    "imaginaryPart(CCi)"
	    "isSubset(CCi,CCi)"
	Headline
	    a class of all complex intervals
	Description
	    Text
	    	The complex interval is entered as a pair of real intervals representing the real and imaginary part respectively.
	    Example
	    	I = intervalCCi(interval(.5,.8),interval(.6,.9))
	    	J = intervalCCi(interval(.54,.78),interval(.65,.89))
		K = intervalCCi(interval(.45,.6),interval(.3,.78))
	    Text
	    	When a pair of numbers are given, it makes an interval with zero radius centered at the given numbers.
	    Example
	    	L = intervalCCi(3,4)
	    Text
	       	Complex interval arithmetic can be applied as defined in the Chapter 9 in @HREF("https://www.degruyter.com/document/doi/10.1515/9783110499469/html?lang=en", "\"Interval Analysis\"")@.
	    Example
	    	I + J
		I - J
		I * K
		I / K
		I ^ 3
		2 * I
	    Text
	    	There are several functions that can be applied on complex intervals.
	    Example
    	    	isSubset(I,J)  -- I is not a subset of J
    	    	isSubset(J,I)  -- J is a subset of I
		intersect(I,K)
		(realPart I, imaginaryPart I)
		norm I
		midpointCCi I

///



doc ///
    	Key
    	    CCiMatrix
	    "matrixCCi"
	    "entries(CCiMatrix)"
	    "transpose(CCiMatrix)"
	    "numColumns(CCiMatrix)"
	    "numRows(CCiMatrix)"
	    "norm(CCiMatrix)"
	Headline
	    a class of matrices of complex intervals
	Description
	    Text
	    	A matrix with complex interval entries can be defined by a nested list.
	    Example
	    	I = intervalCCi(interval(.5,.8),interval(.6,.9));
	    	J = intervalCCi(interval(.54,.78),interval(.65,.89));
		K = intervalCCi(interval(.45,.6),interval(.3,.78));
	    	L = intervalCCi(3,4);
		M = matrixCCi {{I,J},{K,L}}
		N = matrixCCi {{J,K},{L,I}}
		O = matrixCCi {{J,K,L},{I,L+K,J*L}}
	    Text
	       	Basic arithmetic operators can be applied on matrices with complex intervals.
	    Example
    	    	M + N
		M - N
		M * N
		M ^ 2
	    Text
	    	There are several functions that can be applied on matrices with complex intervals.
	    Example
    	    	entries M
    	    	transpose O
		numcols O
		numrows O
		norm O
///











doc ///
    	Key
	    alphaCertified
	    (alphaCertified, PolySystem, List)
	Headline
	    certify a list of numerical solutions via alphaCertified
	Description
	    Text
	    	For a given polynomial system and a list of numerical roots, this function certifies roots using a software @HREF("https://www.math.tamu.edu/~sottile/research/stories/alphaCertified/", "\"alphaCertified\"")@.

		In order to use alphaCertified, users need to let the package knows a directory for alphaCertified when it is loaded.
	    Example
	    	needsPackage("NumericalCertification", Configuration => {"ALPHACERTIFIEDexec" => "some/path/to/alphaCertified"})
	    Text
		Function only takes elements over @TO "RR"@ or @TO "CC"@ as an input.
    	    Example
		R = CC[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -3.5, x1^2 + y1^2 -1, x2^2 + y2^2 -1};
		p1 = point{{.95, .32, -.30, .95}};
		p2 = point{{.65,.77,.76,-.64}};
		P = {p1, p2};
	    Text
	    	The function can be used as {\tt alphaCertified(f,p)}. It generates output files via the software alphaCertified.
	        The output can be found in the user's directory of alphaCertified.

		Users can also apply options for alphaCertified (e.g. {\tt alphaCertified(f, P, PRECISION => 4096)}). For possible options for alphaCertified, see @HREF("https://www.math.tamu.edu/~sottile/research/stories/alphaCertified/Download/V13/alphaCertified.pdf", "\"alphaCertified manual\"")@.




///
