export {"chemequ"}

chemequ = method();
chemequ(Ring) := kk -> (
    y := symbol y;
    R := kk[y_1..y_5];
    { y_1*y_2 + y_1 - 3*y_5,
      2*y_1*y_2 + y_1 + 1.9230e-6*y_2^2 + y_2*y_3^2 + 5.4518e-4*y_2*y_3 + 3.4074e-5*y_2*y_4 + 4.4975e-7*y_2 - 10*y_5,
      2*y_2*y_3^2 + 5.4518e-4*y_2*y_3 + 3.8600e-1*y_3^2 + 4.1062e-4*y_3 - 8*y_5,
      3.4074e-5*y_2*y_4 + 2*y_4^2 - 40*y_5,
      y_1*y_2 + y_1 + 9.6150e-7*y_2^2 + y_2*y_3^2 + 5.4518e-4*y_2*y_3 + 3.4074e-5*y_2*y_4 + 4.4975e-7*y_2 + 1.930e-1*y_3^2 + 4.1062e-4*y_3 + y_4^2 - 1
       }
)

beginDocumentation()

doc /// 
    Key
    	chemequ
	(chemequ,Ring)
    Headline
    	chemical equilibrium of hydrocarbon combustion
    Usage
    	chemequ(kk)
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
	   
	    There were 6 solutions found in 1.24877 seconds (with a Bezout bound of 108).
	    
	    Reference: 
	    Keith Meintjes and Alexander P. Morgan,
 	    "Chemical equilibrium systems as numerical test problems",
	    ACM Toms, Vol 16, No 2, 143-151, 1990.
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/chemequ.html
	Example
	    chemequ(CC_53)
    ///
