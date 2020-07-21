export{"ku10"}

ku10 = method()
ku10 (Ring) := kk -> (
    x := symbol x;
    R := kk[x_1..x_10];
   { 5*x_1*x_2 + 5*x_1 + 3*x_2 + 55,
       7*x_2*x_3 + 9*x_2 + 9*x_3 + 19,
       3*x_3*x_4 + 6*x_3 + 5*x_4 - 4,
       6*x_4*x_5 + 6*x_4 + 7*x_5 + 118,
       x_5*x_6 + 3*x_5 + 9*x_6 + 27,
       6*x_6*x_7 + 7*x_6 + x_7 + 72,
       9*x_7*x_8 + 7*x_7 + x_8 + 35,
       4*x_8*x_9 + 4*x_8 + 6*x_9 + 16,
       8*x_9*x_10 + 4*x_9 + 3*x_10 - 51,
       3*x_1*x_10 - 6*x_1 + x_10 + 5
     }
 )

beginDocumentation()

doc /// 
    Key
    	ku10
	(ku10,Ring)
    Headline
    	a 10-dimensional system of Ku
    Usage
    	ku10(kk)
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
	   
	    There were 4 solutions found in 2.266 seconds (with a Bezout bound of 1024).
	    
	    We point out that this system is difficult for homotopy continuation (without 
		multi-homogenization), yet an easy system for elimination. 
	   
	    Reference: "Die numeriese oplos van stelsels polinoomvergelykings" by M.C. Steenkamp.
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/ku10.html.
	Example
	    ku10(QQ)
    ///