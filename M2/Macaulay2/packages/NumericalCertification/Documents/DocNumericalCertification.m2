-- Documents for Numerical Certification.

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
			This package follows the algorithms of alpha theory introduced in @HREF("https://arxiv.org/abs/1011.1091","\alphaCertified: certifying solutions to polynomial systems\" (2012)")@.
		Text
		    	{\bf Ceritification Methods:}
			
			    $\bullet$ @TO "certifySolution"@
			    
			    $\bullet$ @TO "krawczykOper"@
			    
			{\bf Examples}
			
		            The following example shows how to certify the roots of solutions for the square polynomial system.
			    This example is suggested in @HREF("https://www3.nd.edu/~jhauenst/preprints/hlPolyExp.pdf", 
				"\Certifying solutions to square systems of polynomial-exponential equations\",(2017)")@
			    
			    
			    $\bullet$ alpha theory    
		Example
		    R = QQ[x1,x2,y1,y2]
		    f = polySystem {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2, x1^2 + y1^2 -1, x2^2 + y2^2 -1}
		    p = point{{.95, .32, -.30, .95}}
		Text
		    A point for certification should be given in advance using other system solvers.
		Example
		    certifySolution(f,p)
		Text
		    It shows the result of certification.
		    
		Text
		    Also, if you have other solutions of the system, alpha theory suggests an algorithm for distinguishing these solutions.
		Example
		    p1 = point{{.95,.32,-.30,.95}}
		    p2 = point{{.65,.77,.76,-.64}}
		    certifyDistinctSoln(f,p1,p2)
		
		Text
		    	    $\bullet$ Krawczyk method
		Example
		    R = QQ[x1,x2,y1,y2]
		    f = {3*y1 + 2*y2 -1, 3*x1 + 2*x2 -7/2,x1^2 + y1^2 -1, x2^2 + y2^2 - 1}
		    I1 = interval(.94,.96)
		    I2 = interval(.31,.33)
		    I3 = interval(-.31,-.29)
		    I4 = interval(.94,.96)
		Text
		    Intervals for certification should be given in advance using other system solvers.
		    After that we set the relationships between variables and intervals.
		Example
		    o = intervalOptionList {("x1" => "I1"), ("x2" => "I2"), ("y1" => "I3"), ("y2" => "I4")}
    	    	    krawczykOper(f,o)
		
		
///


