export{"rose"}

rose = method()
rose (Ring) := kk -> (
	(x,y,z) := (symbol x, symbol y, symbol z);
	R := kk[x,y,z];
	{ y^4-20/7*x^2,
	x^2*z^4+7/10*x*z^4+7/48*z^4-50/27*x^2-35/27*x-49/216,
	3/5*x^6*y^2*z+x^5*y^3+3/7*x^5*y^2*z+7/5*x^4*y^3-7/20*x^4*y*z^2-3/20*x^4*z^3+609/1000*x^3*y^3+63/200*x^3*y^2*z-77/125*x^3*y*z^2-21/50*x^3*z^3+49/1250*x^2*y^3+147/2000*x^2*y^2*z-23863/60000*x^2*y*z^2-91/400*x^2*z^3-27391/800000*x*y^3+4137/800000*x*y^2*z-1078/9375*x*y*z^2-5887/200000*x*z^3-1029/160000*y^3-24353/1920000*y*z^2-343/128000*z^3 } )

beginDocumentation()

doc /// 
    Key
    	rose
	(rose,Ring)
    Headline
    	the system Rose, a general economic equilibrium model
    Usage
	rose(kk)
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
	   
	    There were 136 solutions found in 4.954 seconds (with a Bezout bound of 216). 
	    
	    Reference: PoSSo test suite.
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/rose.html.
	Example
	    rose(QQ)
    ///