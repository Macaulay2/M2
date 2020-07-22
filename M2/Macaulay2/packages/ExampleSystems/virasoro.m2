export{"virasoro"}

virasoro = method()
virasoro (Ring) := kk -> (
    x := symbol x;
    R := kk[x_1..x_8];
    { 8*x_1^2+ 8*x_1*x_2+ 8*x_1*x_3+ 2*x_1*x_4+ 2*x_1*x_5+ 2*x_1*x_6+ 2*x_1*x_7-8*x_2*x_3-2*x_4*x_7-2*x_5*x_6-x_1,
 	8*x_1*x_2-8*x_1*x_3+ 8*x_2^2+ 8*x_2*x_3+ 2*x_2*x_4+ 2*x_2*x_5+ 2*x_2*x_6+ 2*x_2*x_7-2*x_4*x_6-2*x_5*x_7-x_2,
	-8*x_1*x_2+ 8*x_1*x_3+ 8*x_2*x_3+ 8*x_3^2+ 2*x_3*x_4+ 2*x_3*x_5+ 2*x_3*x_6+ 2*x_3*x_7-2*x_4*x_5-2*x_6*x_7-x_3,
 	2*x_1*x_4-2*x_1*x_7+ 2*x_2*x_4-2*x_2*x_6+ 2*x_3*x_4-2*x_3*x_5+ 8*x_4^2+ 8*x_4*x_5+ 2*x_4*x_6+2*x_4*x_7+ 6*x_4*x_8-6*x_5*x_8-x_4,
 	2*x_1*x_5-2*x_1*x_6+ 2*x_2*x_5-2*x_2*x_7-2*x_3*x_4+ 2*x_3*x_5+ 8*x_4*x_5-6*x_4*x_8+ 8*x_5^2+2*x_5*x_6+ 2*x_5*x_7+ 6*x_5*x_8-x_5,
	-2*x_1*x_5+ 2*x_1*x_6-2*x_2*x_4+ 2*x_2*x_6+ 2*x_3*x_6-2*x_3*x_7+ 2*x_4*x_6+ 2*x_5*x_6+ 8*x_6^2+ 8*x_6*x_7+ 6*x_6*x_8-6*x_7*x_8-x_6,
	-2*x_1*x_4+ 2*x_1*x_7-2*x_2*x_5+ 2*x_2*x_7-2*x_3*x_6+ 2*x_3*x_7+ 2*x_4*x_7+ 2*x_5*x_7+ 8*x_6*x_7-6*x_6*x_8+ 8*x_7^2+ 6*x_7*x_8-x_7,
	-6*x_4*x_5+ 6*x_4*x_8+ 6*x_5*x_8-6*x_6*x_7+ 6*x_6*x_8+ 6*x_7*x_8+ 8*x_8^2-x_8
	}
 )

beginDocumentation()

doc ///
    Key
    	virasoro
	(virasoro, Ring)
    Headline
    	the construction of Virasoro algebras
    Usage
    	virasoro(kk)
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
	   
	    There were 256 solutions found in 0.356 seconds (with a Bezout bound of 256).
	    
	    Reference: "Generalized Virasoro Constructions for SU(3)" by S. Schrans and W. Troost (pages 584-606).
	        
	    See also: http://homepages.math.uic.edu/~jan/Demo/virasoro.html.
	Example
    	    virasoro(QQ)
   ///