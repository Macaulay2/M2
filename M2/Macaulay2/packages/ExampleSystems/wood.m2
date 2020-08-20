export{"wood"}

wood = method()
wood (Ring) := kk -> (
    x := symbol x;
    R := kk[x_1..x_4];
    {  200*x_1^3-200*x_1*x_2+x_1-1,
	-100*x_1^2+ 1.10100000000000e02*x_2+ 9.9*x_4-20,
 	180*x_3^3-180*x_3*x_4+x_3-1,
	-90*x_3^2+ 9.9*x_2+ 1.00100000000000e02*x_4-20
	}
 )

beginDocumentation()

doc ///
    Key
    	wood
	(wood, Ring)
    Headline
    	system derived from optimizing the Wood function
    Usage
    	wood(kk)
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
	   
	    There were 9 solutions found in 0.141 seconds (with a Bezout bound of 36).
	    
	    Reference: "Testing unconstrained optimization software" by J.J. More, B.S. Garbow and K.E. Hilstrom (pages 17-41).
	        
	    See also: http://homepages.math.uic.edu/~jan/Demo/wood.html.
	Example
    	    wood(RR_53)
   ///