-- Documents for Numerical Certification.



-- These are methods/functions only used as a mechanism. (will not be used by users)
undocumented{(invmat, PolySystem, IntervalOptionList), (net, Interval),
    (substitute, Interval, IntervalOptionList), (substitute, Interval, StringOption),
    (substitute, Number, StringOption), (substitute, RingElement, IntervalOptionList),
    (substitute, RingElement, StringOption), stringOption, StringOption, invmat, 
    krawczykUniqueness, (krawczykUniqueness, PolySystem, IntervalOptionList)}


-- These are already documented as methods.
undocumented{mInterval, wInterval, intervalNorm} 



-- These interval arithmetic methods are documented in the documents of its classes.
undocumented{(symbol +, Interval, Interval), (symbol *, Interval, RingElement), 
    (symbol *, Interval, Interval), (symbol *, RingElement, Interval),
    (symbol *, Number, Interval), (symbol ^, Interval, Number),
    (symbol ^, Number, Interval), (symbol *, IntervalMatrix, IntervalMatrix),
    (symbol -, Interval, Interval), (symbol /, Interval, Interval)}



doc ///
    	Key
	    	NumericalCertification
	Headline
	    	certify the solution for the square system using alpha theory or interval arithmetic
	Description
	    	Text
		    	This package provides two different types of root certification for the square polynomial system.
		      	The first is Smale's alpha theory and the second is Krawczyk method via interval arithmetic.
			Both methods are based on Newton's method and they all describe the specific region containing a unique root of the system.
			
			In the case of alpha theory, this package follows the algorithms of alpha theory introduced in @HREF("https://arxiv.org/abs/1011.1091","\"alphaCertified: certifying solutions to polynomial systems\" (2012)")@.
			
			In the case of Krawczyk method, this package follows the theory suggested in @HREF("https://epubs.siam.org/doi/book/10.1137/1.9780898717716","\"Introduction to Interval Analysis\" (2009)")@.
			    
		Text
		    	{\bf Ceritification Methods:}
			
			    $\bullet$ @TO "certifySolution"@
			    
			    $\bullet$ @TO "krawczykMethod"@
			    
			{\bf Examples}
			
		            The following example shows how to certify the roots of solutions for the square polynomial system.
			    This example is suggested in @HREF("https://www3.nd.edu/~jhauenst/preprints/hlPolyExp.pdf", 
				"\"Certifying solutions to square systems of polynomial-exponential equations\" (2017)")@
			    
			    
			{\bf    $\bullet$ alpha theory}    
		Text
		    A set of points for certification should be given in advance using other system solvers.
		Example
		    R = QQ[x1,x2,y1,y2];
		    f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2, x1^2 + y1^2 -1, x2^2 + y2^2 -1};
		    p1 = point{{.95, .32, -.30, .95}};
		    p2 = point{{.9, .3, -.3, 1}}; -- poorly approximated solution
		    P = {p1,p2};
		Text
		    It shows the result of certification.
		Example
		    certifySolution(f,P)
		    
		Text
		    Also, if we have other solutions of the system, alpha theory suggests an algorithm for distinguishing these solutions.
		Example
		    p1 = point{{.95,.32,-.30,.95}};
		    p2 = point{{.65,.77,.76,-.64}};
		    certifyDistinctSoln(f,p1,p2)
		Text
		    In the case of real polynomial system, we can certify that a given solution is real or not.
		Example
		    p = point{{.954379, .318431, -.298633, .947949}};
		    certifyRealSoln(f,p)
		
		Text
		    {\bf    $\bullet$ Krawczyk method}
		Text
		    Intervals for certification should be given in advance using other system solvers.
		Example
		    R = QQ[x1,x2,y1,y2];
		    f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2, x1^2 + y1^2 -1, x2^2 + y2^2 -1};
		    (I1, I2, I3, I4) = (interval(.94,.96), interval(.31,.33), interval(-.31,-.29), interval(.94,.96));
		Text
		    We set the relationships between variables and intervals using the function @TO "intervalOptionList"@.
		Example
		    o = intervalOptionList {("x1" => "I1"), ("x2" => "I2"), ("y1" => "I3"), ("y2" => "I4")};
    	    	    krawczykOper(f,o)
		Text
		    The function @TO "krawczykMethod"@ automatically checks whether the Krawczyk operator is contained in the input interval box.
		Example
		    krawczykMethod(f,o)
		
		
///



doc ///
    	Key
	    pointNorm
	    (pointNorm, Point)
	    "(pointNorm, Point)"
	Headline
	    compute the "projectivized" norm of the given point
	Usage
	    n = pointNorm(p)
	Inputs
	    p:Point
	Description
	    Text
	    	For the given point this function computes the "projectivized" norm (square root of $1+ x_1^2 + ... + x_n^2$) of the given point.
		This will be used in order to compute the gamma value.
	    Example
	    	p = point {{.2,3}};
		pointNorm(p)
///


doc ///
    	Key
	    polyNorm
	    (polyNorm, RingElement)
	    "(polyNorm, RingElement)"
	Headline
	    compute the "Bombieri-Weyl" norm of the given polynomial
	Description
	    Text
	    	For the given point this function computes the "Bombieri-Weyl" norm of the given point.
		This will be used in order to compute the gamma value.
	    Example
	    	R = QQ[x,y];
		f = 3*x*y^2 + 3*x + 7*y^2;
		polyNorm(f)
///
		

doc ///
    	Key
	    polySysNorm
	    (polySysNorm, PolySystem)
	    "(polySysNorm, PolySystem)"
	Headline
	    compute the norm of the given polynomial system
	Usage
	    polySysNorm(PS)
	Inputs
	    PS:PolySystem
	Description
	    Text
	    	For the given polynomial system we define the polynomial system norm by square root of the sum of all squares of polynomial norms.
		This will be used in order to compute the gamma value.
	    Example
	        R = QQ[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
		polySysNorm(f)
///		
	       
	       
doc ///
    	Key
    	    newtonOper
	    (newtonOper, PolySystem, Point)
	    "(newtonOper, PolySystem, Point)"
	Headline
    	    apply Newton's method on a given point.
	Usage
    	    newtonOper(PS, P)
	Inputs
            PS:PolySystem
	    P:Point
	Description
	    Text
    	    	This function applies Newton's method on a given point and given polynomial system.
		
		It converges to an exact solution if a given point is an approximate solution.
	    Example
	        R = QQ[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
		p = point{{.95,.32,-.30,.95}};
    	    	newtonOper(f,p)
    	    Text
	    	However, if the Jacobian of the system is singular at the point, it just returns the input.
	    Example
	    	p = point{{0,.32,0,.95}};
		newtonOper(f,p)		
///		


	       
	       
doc ///
    	Key
    	    computeConstants
	    (computeConstants, PolySystem, Point)
	    "(computeConstants, PolySystem, Point)"
	Headline
	    compute the auxiliary quantities related to alpha theory
	Usage
	    (alpha, beta, gamma) = computeConstants(PS, P)
	Inputs
            PS:PolySystem
	    P:Point
	Description
	    Text
    	    	alpha theory uses three auxiliary quantities related to the input polynomial system and point.

		Beta value is defined by the length of the Newton step and gamma value is the quantity which is inversely proportional to the length between exact solution and the given point.

		Alpha value is defined by the multiplication of beta and gamma. When it is smaller than  $0.157671$, then the input point is an approximate solution to the system. The function @TO "certifySolution"@ does this process.
	    Example
	        R = QQ[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
		p = point{{.95,.32,-.30,.95}};
    	    	(a, b, g) = computeConstants(f,p)
///		


	       
doc ///
    	Key
    	    certifySolution
	    (certifySolution, PolySystem, List)
	    "(certifySolution, PolySystem, List)"
	Headline
	    certify whether a given point is an approximate solution to the system
	Usage
	    (points, constants) = certifySolution(PS, P)
	Inputs
            PS:PolySystem
	    P:List
	Outputs
	    points:List
	          a list of certified solutions
	    constants:List
	          a list of constants for each certified solution
	Description
	    Text
    	    	This function executes the alpha test based on the value computed by @TO "computeConstants"@.
	    Example
	        R = QQ[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
	    Text
	    	Input should be a list of @TO "Point"@ type objects.
	    Example
		p = point{{.95,.32,-.30,.95}};
    	    	certifySolution(f,{p})
///		



doc ///
    	Key
    	    certifyDistinctSoln
	    (certifyDistinctSoln, PolySystem, Point, Point)
	    "(certifySolution, PolySystem, Point, Point)"
	Headline
	    determine whether given points are distinct approximate solutions to the system
	Usage
	    certifyDistinctSoln(PS, P1, P2)
	Inputs
            PS:PolySystem
	    P1:Point
	    P2:Point
	Description
	    Text
    	    	This function executes the gamma test based on the value computed by @TO "computeConstants"@, and determine whether given points are distinct or not.
	    Example
	        R = QQ[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
		p1 = point{{.95,.32,-.30,.95}};
		p2 = point{{.65,.77,.76,-.64}};
    	    	certifyDistinctSoln(f,p1,p2)
	    Text
	    	However, if two solutions are too close, it concludes that inputs are not disticnt.
	    Example
		p1 = point{{.6525,.7712,.7577,-.6366}};
		p2 = point{{.653,.771,.758,-.637}};
    	    	certifyDistinctSoln(f,p1,p2)
	    Text
	    	Even worse, if two solutions are close enough and both have alpha value which are bigger than $0.03$, it gives indecisive comments.
		
		In this case, user should apply @TO "newtonOper"@ to the point to get more precise approximation.
	    Example
		p1 = point{{.95,.32,-.30,.95}};
		p2 = point{{.95,.32,-.301,.95}};
    	    	certifyDistinctSoln(f,p1,p2)
///		



doc ///
    	Key
    	    certifyRealSoln
	    (certifyRealSoln, PolySystem, Point)
	    "(certifyRealSoln, PolySystem, Point)"
	Headline
	    determine whether a given point is an real approximate solution to the system
	Usage
	    certifyDistinctSoln(PS, P)
	Inputs
            PS:PolySystem
	    P:Point
	Description
	    Text
    	    	When the system is real (or rational) polynomial system, this function executes the gamma test based on the value computed by @TO "computeConstants"@, and determine whether a given point is a real approximate solution  or not.
	    Example
	        R = QQ[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
		p = point{{.954379,.318431,-.298633,.947949}};
    	    	certifyRealSoln(f,p)
	    Text
	    	However, an input point is poorly approximated, it gives false even if the point is real.
		In this case, user should apply @TO "newtonOper"@ to the point to get more precise approximation.
	    Example
		p = point{{.65,.77,.75,-.64}};  -- poorly approximated solution
    	    	certifyRealSoln(f,p)
///		



doc ///
    	Key
    	    certifyCount
	    (certifyCount, PolySystem, List)
	    "(certifyCount, PolySystem, List)"
	Headline
    	    executes alpha-certification on a given system and list of points
	Usage
	    (D, R, CS, C) = certifyCount(PS, P)
	Inputs
            PS:PolySystem
	    P:Point
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
	    	When the system solved by solver has lots of points, this function does all procedures of @TO "certifySolution"@, @TO "certifyDistinctSoln"@ and @TO "certifyRealSoln"@ at once.
	    Example
	        R = QQ[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
		p1 = point{{.954379,.318431,-.298633,.947949}}; p2 = point{{.95, .32, -.30, .95}}; p3 = point{{.652567, .77115, .757776, -.636663}}; p4 = point{{.65, .77, .76, -.64}}; 
		p5 = point{{.31, .30, .72, -.60}}; -- poorly approximated solution
		P = {p1, p2, p3, p4, p5}
    	    	certifyCount(f,P)
///		






doc ///
    	Key
	    Interval
	Headline
	    a class of all intervals
	Description
	    Text
	    	This type is a new type of @TO "List"@. The function @TO "interval"@ can be used to access an @TO "Interval"@.
	    Example
	    	I = interval(.5,.8)
	    Text	
	    	Users can make an interval which has polynomials as entries.
	    Example
	    	R = QQ[x];
		J = interval(5,3*x)
	    Text
	    	@TO "NumericalCertification"@ supports some basic interval arithmetics.
	    Example
	    	I1 = interval(.5,.8);
		I2 = interval(.6,.9);
		I1 + I2
		I1 - I2
		I1 * I2
		I1 / I2
		I1 ^ 3

///

doc ///
    	Key
	    IntervalMatrix
	Headline
	    a class of all interval matrices
	Description
	    Text
	    	This type is a new type of @TO "List"@, and it works as a matrix with @TO "interval"@ entries.
    	    	
		The function @TO "intervalMatrix"@ can be used to access this type.
	    Example
	    	m = intervalMatrix {{interval(1,2), interval(2,3)},{interval(3,4),interval(1,3)}}
	    Text
	    	@TO "NumericalCertification"@ supports the multiplication of interval matrices.
	    Example
	    	n = intervalMatrix {{interval(1,2), interval(.2,.5)},{interval(2,3),interval(-2,-1)}};
		m*n
///


doc ///
    	Key
	    IntervalOptionList
	Headline
	    a class of lists for options related to intervals
	Description
	    Text
	    	This type is a new type of @TO "List"@, and it works when @TO "krawczykOper"@ and @TO "krawczykMethod"@ assign intervals to variables in the system.
		
		@TO "krawczykOper"@ substitute given intervals into variables in the system as the way @TO "IntervalOptionList"@ suggests.
    	    	
    	    	@TO "krawczykOper"@ and @TO "krawczykMethod"@ doesn't work with @TO "List"@ type input. Thus, users should change the list of options into @TO "IntervalOptionList"@.
		
		The function @TO "intervalOptionList"@ can be used to convert a @TO "List"@ type object into @TO "IntervalOptionList"@.
	    Example
                R = QQ[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2, x1^2 + y1^2 -1, x2^2 + y2^2 -1};
		(I1, I2, I3, I4) = (interval(.94,.96), interval(.31,.33), interval(-.31,-.29), interval(.94,.96));
	       	intervalOptionList {("x1" => "I1"), ("x2" => "I2"), ("y1" => "I3"), ("y2" => "I4")}
///

		

doc ///
    	Key
	    interval
	    (interval, Number)
	    (interval, Number, Number)
	    (interval, Number, RingElement)
	    (interval, RingElement, Number)
	    (interval, RingElement, RingElement)
	    (interval, RingElement, Interval)
	    (interval, Interval)
	Headline
	    construct an interval
	Description
	    Text
	        This function is used to access a type @TO "Interval"@.
	    Example
	    	J = interval(3)
	        I = interval(.5,.8)
	    Text	
	    	Users can make an interval which has polynomials as entries.
	    Example
	    	R = QQ[x];
		J = interval(5,3*x)
    	    Text
	    	The type @TO "Interval"@ can also be an input for the @TO "interval"@.
	    Example
	        I = interval(.5,.8)
	    	J = interval I
///


doc ///
    	Key
	    intervalMatrix
	    (intervalMatrix, List)
	Headline
	    construct an interval matrix from the given list
	Description
	    Text
	        This function is used to access a type @TO "IntervalMatrix"@ from the @TO "List"@.
	    Example
	    	l = {{interval(1,2), interval(2,3)},{interval(3,4),interval(1,3)}}
	    	m = intervalMatrix l
///


doc ///
    	Key
	    intervalMatrixNorm
	    (intervalMatrixNorm, IntervalMatrix)
	Headline
	    compute the infinity norm for interval matrix.
	Description
	    Text
	        This function computes the infinity norm for a given interval matrix
	    Example
	    	l = intervalMatrix {{interval(1,2), interval(2,3)},{interval(3,4),interval(1,3)}};
	    	intervalMatrixNorm l
///




doc ///
    	Key
	    intervalOptionList
	    (intervalOptionList, List)
	Headline
	    convert a list type object to list of options for intervals
	Description
	    Text
	    	This function converts a @TO "List"@ type object to a @TO "IntervalOptionList"@ type object.

    	    	First, assume that we have the following ring, polynomial system, and interval. 
	    Example
                R = QQ[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2, x1^2 + y1^2 -1, x2^2 + y2^2 -1};
		(I1, I2, I3, I4) = (interval(.94,.96), interval(.31,.33), interval(-.31,-.29), interval(.94,.96));
    	    Text
	    	We want to plug in "I1" into "x1", "I2" into "x2", "I3" into "y1" and "I4" into "y2".
		
		Then, write a @TO "List"@ object as the way we want.
	    Example
    	    	l = {("x1" => "I1"), ("x2" => "I2"), ("y1" => "I3"), ("y2" => "I4")}
	    Text
	    	Then, use @TO "intervalOptionList"@ function to convert the type of "l" to @TO "IntervalOptionList"@.
	    Example
	       	intervalOptionList l 
///


	    
	    
doc ///
    	Key
	    (wInterval, Interval)
	Headline
	    compute a width of an interval.
	Description
	    Text
    	    	We define the width of an interval as a difference between a upper bound and lower bound of an interval.
	    Example
	        I = interval(.5,.8);
		wInterval I
///
	    
	    
doc ///
    	Key
	    (mInterval, Interval)
	Headline
	    compute a midpoint of an interval
	Description
	    Example
	        I = interval(.5,.8);
		mInterval I
///
	    
	    
doc ///
    	Key
	    (intervalNorm, Interval)
	Headline
	    compute a norm of an interval
	Description
	    Text
	    	We define the norm of an interval as the maximum element in the interval.
	    Example
	        I = interval(.5,.8);
		intervalNorm I
///
	    
	    
	    
doc ///
    	Key
	    identityIntMat
	    (identityIntMat, ZZ)
	Headline
	    compute the identity diagonal interval matrix. 
	Description
	    Text
	    	A diagonal matrix with "[1,1]" as a diagonal entry is worked as a identity matrix in interval arithmetic.
		
		For given positive integer "n", this function constructs the identity interval matrix of size "n".
	    Example
    	    	identityIntMat 3
///
	    
	    
doc ///
    	Key
	    krawczykMethodOptions
	    InvertibleMatrix
	    [krawczykOper, InvertibleMatrix]
	    [krawczykMethod, InvertibleMatrix]
      	Headline
	    invertible matrix for computing Krawczyk operator (option for "krawczykOper" and "krawczykMethod")
	Description
	    Example
	    	options krawczykMethod
	    Text
	    	This is an option for @TO "krawczykOper"@ and @TO "krawczykMethod"@. By default, these functions uses the inverse of the matrix obtained by evaluating the system at the midpoint of the input interval.
       	       	
		Input for this option have to be an invertible matrix.
	    Example
	    	R = QQ[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
	        (I1, I2, I3, I4) = (interval(.94,.96), interval(.31,.33), interval(-.31,-.29), interval(.94,.96));
		o = intervalOptionList {("x1" => "I1"), ("x2" => "I2"), ("y1" => "I3"), ("y2" => "I4")};
		Y = matrix {{.095, .032, .476, -.100},{-.143, .452, -.714, .150},{.301,.101,-.160, -.317},{.048,-.152,.240,.476}};
    	    	krawczykMethod(f,o,InvertibleMatrix => Y)		
///    

doc ///
    	Key
	    krawczykOper
	    (krawczykOper, PolySystem, IntervalOptionList)
	Headline
	    compute the Krawczyk operator
	Description
	    Text
	    	For given interval and polynomial system, this function computes the Krawczyk operator.
	    Example
	    	R = QQ[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
	        (I1, I2, I3, I4) = (interval(.94,.96), interval(.31,.33), interval(-.31,-.29), interval(.94,.96));
	    Text
	        Intervals for certification should be given in advance using other system solvers.
	        After that we set the relationships between variables and intervals.
	    Example
		o = intervalOptionList {("x1" => "I1"), ("x2" => "I2"), ("y1" => "I3"), ("y2" => "I4")};
    	    Text
	    	If the Krawczyk operator is contained in the input interval, then we conclude that the input interval (or the Krawczyk operator) contains a unique root of the system.
	    Example
    	    	krawczykOper(f,o)
    	    Text
		The function @TO "krawczykMethod"@ checks this criterion automatically.

    	        By default, these functions uses the inverse of the matrix obtained by evaluating the system at the midpoint of the input interval.
		
		However, users can choose an invertible matrix by themselves. (See @TO "InvertibleMatrix"@.)
	    Example
	    	Y = matrix {{.095, .032, .476, -.100},{-.143, .452, -.714, .150},{.301,.101,-.160, -.317},{.048,-.152,.240,.476}};
    	    	krawczykOper(f,o,InvertibleMatrix => Y)	
		
///


doc ///
    	Key
	    krawczykMethod
	    (krawczykMethod, PolySystem, IntervalOptionList)
	Headline
	    certify the interval box for square polynomial system
	Description
	    Text
	    	For given interval and polynomial system, this function computes the Krawczyk operator and check that the operator is contained in the input interval.
	    Example
	    	R = QQ[x1,x2,y1,y2];
		f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1};
	        (I1, I2, I3, I4) = (interval(.94,.96), interval(.31,.33), interval(-.31,-.29), interval(.94,.96));
	    Text
	        Intervals for certification should be given in advance using other system solvers.
	        After that we set the relationships between variables and intervals.
	    Example
		o = intervalOptionList {("x1" => "I1"), ("x2" => "I2"), ("y1" => "I3"), ("y2" => "I4")};
	    Text
	    	If the Krawczyk operator is contained in the input interval, then the function returns the result that the input interval (or the Krawczyk operator) contains a unique root of the system.
	    Example
    	    	krawczykMethod(f,o)
    	    Text
    	        By default, these functions uses the inverse of the matrix obtained by evaluating the system at the midpoint of the input interval.
		
		However, users can choose an invertible matrix by themselves. (See @TO "InvertibleMatrix"@.)
	    Example
	    	Y = matrix {{.095, .032, .476, -.100},{-.143, .452, -.714, .150},{.301,.101,-.160, -.317},{.048,-.152,.240,.476}};
    	    	krawczykMethod(f,o,InvertibleMatrix => Y)	
		

		
///



