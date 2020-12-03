export{"reimer5"}

reimer5 = method()
reimer5 (Ring) := kk -> (
	(x,y,z,t,u) := (symbol x, symbol y, symbol z, symbol t, symbol u);
	R := kk[x,y,z,t,u];
	{ -1 + 2*x^2 - 2*y^2 + 2*z^2 - 2*t^2 + 2*u^2,
	-1 + 2*x^3 - 2*y^3 + 2*z^3 - 2*t^3 + 2*u^3,
	-1 + 2*x^4 - 2*y^4 + 2*z^4 - 2*t^4 + 2*u^4,
	-1 + 2*x^5 - 2*y^5 + 2*z^5 - 2*t^5 + 2*u^5,
	-1 + 2*x^6 - 2*y^6 + 2*z^6 - 2*t^6 + 2*u^6 } )

beginDocumentation()

doc /// 
    Key
    	reimer5
	(reimer5,Ring)
    Headline
    	5-dimensional Reimer system
    Usage
	reimer5(kk)
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
	   
	    There were 146 solutions found in 6.540 seconds (with a Bezout bound of 720). 
	    
	    Reference: PoSSo test suite.
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/reimer5.html.
	Example
	    reimer5(QQ)
    ///