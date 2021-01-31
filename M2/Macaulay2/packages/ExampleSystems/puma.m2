export{"puma"}

puma = method()
puma (Ring) := kk -> (
	x := symbol x;
	R := kk[x_1..x_8];
	{ x_1^2 + x_2^2 - 1,
	x_3^2 + x_4^2 - 1,
	x_5^2 + x_6^2 - 1,
	x_7^2 + x_8^2 - 1,
	0.004731*x_1*x_3 - 0.3578*x_2*x_3 - 0.1238*x_1 - 0.001637*x_2 - 0.9338*x_4 + x_7 - 0.3571,
	0.2238*x_1*x_3 + 0.7623*x_2*x_3 + 0.2638*x_1 - 0.07745*x_2 -0.6734*x_4 -0.6022,
	x_6*x_8 + 0.3578*x_1 + 0.004731*x_2,
	-0.7623*x_1 + 0.2238*x_2 + 0.3461 }
 )

beginDocumentation()

doc /// 
    Key
    	puma
	(puma,Ring)
    Headline
    	hand position and orientation of PUMA robot
    Usage
	puma(kk)
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
	   
	    There were 16 solutions found in 0.531 seconds (with a Bezout bound of 128). 
	    
	    Reference: "Box-Bisection for solving second-degree systems and the problem of clustering', ACM Transactions on Mathematical Software" by A. Morgan and V. Shapiro. (p. 152-167).
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/puma.html.
	Example
	    puma(RR_53)
    ///