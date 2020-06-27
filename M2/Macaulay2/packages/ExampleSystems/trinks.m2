export{"trinks"}

trinks = method()
trinks (Ring) := kk -> (
    (x, y, z, t, u, v) := (symbol x, symbol y, symbol z, symbol t, symbol u, symbol v);
    R := kk[x, y, z, t, u, v];
    {45*y + 35*u - 165*v - 36,
 	35*y + 25*z + 40*t - 27*u,
 	25*y*u - 165*v^2 + 15*x - 18*z + 30*t,
 	15*y*z + 20*t*u - 9*x,
 	-11*v^3 + x*y + 2*z*t,
 	-11*u*v + 3*v^2 + 99*x
	}
 )

beginDocumentation()

doc ///
    Key
    	trinks
	(trinks, Ring)
    Headline
    	system of Trinks from the PoSSo test suite
    Usage
    	trinks(kk)
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
	   
	    There were 10 solutions found in 0.203 seconds (with a Bezout bound of 24).
	    
	    Reference: See the PoSSo test suite.
	        
	    See also: http://homepages.math.uic.edu/~jan/Demo/trinks.html.
        Example
    	    trinks(QQ)
   ///