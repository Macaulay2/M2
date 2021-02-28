export{"noon5"}

noon5 = method()
noon5(Ring) := kk -> (
    x := symbol x;
    R := kk[x_1..x_5];
    {x_1*x_2^2 + x_1*x_3^2 + x_1*x_4^2 + x_1*x_5^2 - 1.1*x_1 + 1,
	x_2*x_1^2 + x_2*x_3^2 + x_2*x_4^2 + x_2*x_5^2 - 1.1*x_2 + 1,
	x_3*x_1^2 + x_3*x_2^2 + x_3*x_4^2 + x_3*x_5^2 - 1.1*x_3 + 1,
	x_4*x_1^2 + x_4*x_2^2 + x_4*x_3^2 + x_4*x_5^2 - 1.1*x_4 + 1,
	x_5*x_1^2 + x_5*x_2^2 + x_5*x_3^2 + x_5*x_4^2 - 1.1*x_5 + 1 }
 )

beginDocumentation()

doc ///
    Key
    	noon5
	(noon5,Ring)
    Headline
    	A neural network modeled by an adaptive Lotka-Volterra system with 5 variables
    Usage
    	noon5(kk)
    Inputs
    	kk:Ring
	    	the coefficient ring
    Outputs
    	:List
	    	of the polynomials in the system
    Description
    	Text
	    This system was solved in May 2020, using @TO solveSystem@ in Macaulay2 v1.15
	     with an Intel(R) Core(TM) i5-5250U CPU at 1.60GHz.
	   
	    There were 233 solutions found in 0.582 seconds (with a Bezout bound of 243).
	    
	    The coefficients have been chosen so that full permutation symmetry was obtained.
	    
	    Reference: "A neural network modeled by an adaptive Lotka-Volterra system" by V.W. Noonburg.
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/noon5.html.
	Example
	    noon5(RR_53)
 ///