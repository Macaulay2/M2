export{"lorentz"}

lorentz = method()
lorentz (Ring) := kk -> (
    x := symbol x;
    R := kk[x_1..x_4];
   { x_1*x_2-x_1*x_3-x_4+ 1,
x_2*x_3-x_2*x_4-x_1+ 1,
-x_1*x_3+x_3*x_4-x_2+ 1,
x_1*x_4-x_2*x_4-x_3+ 1
        }
 )

beginDocumentation()

doc ///
    Key
    	lorentz
	(lorentz, Ring)
    Headline
    	equilibrium points of a 4-dimensional Lorentz attractor
    Usage
    	lorentz(kk)
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
	   
	    There were 12 solutions found in 0.0562 seconds (with a Bezout bound of 16).
	    
	    Reference: "Solving polynomial systems" by Tien-Yien Li (pages 33-39).
	        
	    See also: http://homepages.math.uic.edu/~jan/Demo/lorentz.html.
	Example
	    lorentz(QQ)
   ///