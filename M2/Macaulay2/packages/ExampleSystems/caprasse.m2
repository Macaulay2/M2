export {"caprasse"}

caprasse = method();
caprasse(Ring) := kk -> (
    (x,y,z,t) := (symbol x, symbol y, symbol z, symbol t);
    R := kk[x,y,z,t];
    { y^2*z+2*x*y*t-2*x-z,
      -x^3*z+4*x*y^2*z+4*x^2*y*t+2*y^3*t+4*x^2-10*y^2+4*x*z-10*y*t+2,
      2*y*z*t+x*t^2-x-2*z,
      -x*z^3+4*y*z^2*t+4*x*z*t^2+2*y*t^3+4*x*z+4*z^2-10*y*t-10*t^2+2
       }
)

beginDocumentation()

doc /// 
    Key
    	caprasse
	(caprasse,Ring)
    Headline
    	the system caprasse of the PoSSo test suite
    Usage
    	caprasse(kk)
    Inputs
    	kk:Ring
	    the coefficient ring
    Outputs
    	:List	
	    of the polynomials in the system
    Description
    	Text
	   This system was solved in May 2020, using @TO solveSystem@ in Macaulay2 v1.15
	     with an Intel(R) Core(TM) i5-4258U CPU at 2.40GHz.
	   
	    There were 64 solutions found in 0.525506 seconds (with a Bezout bound of 144).
	    
	    Reference: 
	    
	    The PoSSo test suite.
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/caprasse.html
	Example
	    caprasse(QQ)
    ///
