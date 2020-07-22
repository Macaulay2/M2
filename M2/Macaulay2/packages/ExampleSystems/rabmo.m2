export{"rabmo"}

rabmo = method()
rabmo (Ring) := kk -> (
	x := symbol x;
	R := kk[x_1..x_9];
	{ x_1 + x_3 + x_5 + 2*x_7 - 1,
	x_1*x_2 + x_3*x_4 + 2*x_5*x_6 + 2*x_7*x_8 + 2*x_7*x_9 - 2/3,
	x_1*x_2^2 + x_3*x_4^2 + 2*x_5*x_6^2 + 2*x_7*x_8^2 + 2*x_7*x_9^2 - 2/5,
	x_1*x_2^3 + x_3*x_4^3 + 2*x_5*x_6^3 + 2*x_7*x_8^3 + 2*x_7*x_9^3 - 2/7,
	x_1*x_2^4 + x_3*x_4^4 + 2*x_5*x_6^4 + 2*x_7*x_8^4 + 2*x_7*x_9^4 - 2/9,
	x_5*x_6^2 + 2*x_7*x_8*x_9 - 1/9,
	x_5*x_6^4 + 2*x_7*x_8^2*x_9^2 - 1/25,
	x_5*x_6^3 + x_7*x_8*x_9^2 + x_7*x_8^2*x_9 - 1/15,
	x_5*x_6^4 + x_7*x_8*x_9^3 + x_7*x_8^3*x_9 - 1/21 }
 )

beginDocumentation()

doc /// 
    Key
    	rabmo
	(rabmo,Ring)
    Headline
    	optimal multi-dimensional quadrature formulas
    Usage
	rabmo(kk)
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
	   
	    There were 58 solutions found in 1642.07 seconds (with a Bezout bound of 36000). 
	    
	    Reference: "Methods and applications of interval analysis" by Ramon E. Moore (p. 64).
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/rabmo.html.
    	Example
	    rabmo(QQ)
 ///
