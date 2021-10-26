export{"lumped"}

lumped = method()
lumped (Ring) := kk -> (
    x := symbol x;
    R := kk[x_1..x_4];
   { ( 7.67718790000000e-01 + 3.28202780000000e-01*ii)*x_1*x_3+( 7.67718790000000e-01 + 3.28202780000000e-01*ii)*x_1*x_4+(-1.76771879000000 - 3.28202780000000e-01*ii)*x_1+( 5.48909490000000e-01 + 1.09394900000000e-01*ii)*x_3+( 4.79608000000000e-02 + 8.88686780000000e-01*ii),
       ( 3.30100210000000e-01 + 8.90584170000000e-01*ii)*x_2*x_3+( 3.30100210000000e-01 + 8.90584170000000e-01*ii)*x_2*x_4+(-1.33010021000000 - 8.90584170000000e-01*ii)*x_2+( 1.11290920000000e-01 + 6.71774920000000e-01*ii)*x_4+( 8.29151510000000e-01 + 6.69877470000000e-01*ii),
       (-7.67718790000000e-01 - 3.28202780000000e-01*ii)*x_1*x_3+(-7.67718790000000e-01 - 3.28202780000000e-01*ii)*x_1*x_4+(-8.92481630000000e-01 - 4.52965620000000e-01*ii)*x_3*x_4+( 7.67718790000000e-01 + 3.28202780000000e-01*ii)*x_1+(-5.48909490000000e-01 - 1.09394900000000e-01*ii)*x_3,
       (-3.30100210000000e-01 - 8.90584170000000e-01*ii)*x_2*x_3+(-3.30100210000000e-01 - 8.90584170000000e-01*ii)*x_2*x_4+(-8.92481630000000e-01 - 4.52965620000000e-01*ii)*x_3*x_4+( 3.30100210000000e-01 + 8.90584170000000e-01*ii)*x_2+(-1.11290920000000e-01 - 6.71774920000000e-01*ii)*x_4
        }
 )

   beginDocumentation()

doc /// 
    Key
    	lumped
	(lumped,Ring)
    Headline
    	lumped parameter chemically reacting system
    Usage
    	lumped(kk)
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
	   
	    There were 4 solutions found in 0.178 seconds (with a Bezout bound of 16).
	    
	    Reference: "Solving deficient polynomial systems with homotopies which keep 
	    the subschemes at infinity invariant" by T.Y. Li and X. Wang (pages 693-710).
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/lumped.html.
	Example
	    lumped(CC_53)
    /// 