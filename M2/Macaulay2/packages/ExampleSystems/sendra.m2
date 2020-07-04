export{"sendra"}

sendra = method()
sendra (Ring) := kk -> (
	(x,y) := (symbol x, symbol y);
	R := kk[x,y];
	{ -270*x^4*y^3-314*x*y^4-689*x*y^3+1428,
	36*x^7+417*x^6*y-422*x^5*y^2-270*x^4*y^3+1428*x^3*y^4-1475*x^2*y^5+510*x*y^6-200*x^6-174*x^5*y-966*x^4*y^2+529*x^3*y^3+269*x^2*y^4+49*x*y^5-267*y^6+529*x^4*y+1303*x^2*y^3-314*x*y^4+262*y^5+36*x^4-788*x^2*y^2-689*x*y^3+177*y^4 } )

beginDocumentation()

doc /// 
    Key
    	sendra
	(sendra,Ring)
    Headline
    	the system sendra
    Usage
	sendra(kk)
    Inputs
	kk:Ring
		the coefficient ring
    Outputs
	:List
		of polynomials in the system
    Description
    	Text
	    This system was solved in May 2020, using @TO solveSystem@ in Macaulay2 v1.15
	     with an Intel(R) Core(TM) i5-5250U CPU at 1.60GHz.
	   
	    There were 46 solutions found in 0.111 seconds (with a Bezout bound of 49). 
	    
	    Reference: PoSSo test suite.
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/sendra.html.
	Example
	    sendra(QQ)
    ///