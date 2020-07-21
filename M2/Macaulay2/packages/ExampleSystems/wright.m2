export{"wright"}

wright = method()
wright (Ring) := kk -> (
    x := symbol x;
    R := kk[x_1..x_5];
    {  x_1^2-x_1+x_2+x_3+x_4+x_5-10,
	x_2^2+x_1-x_2+x_3+x_4+x_5-10,
	x_3^2+x_1+x_2-x_3+x_4+x_5-10,
	x_4^2+x_1+x_2+x_3-x_4+x_5-10,
	x_5^2+x_1+x_2+x_3+x_4-x_5-10
	}
 )

beginDocumentation()

doc ///
    Key
    	wright
	(wright, Ring)
    Headline
    	system of A.H. Wright
    Usage
    	wright(kk)
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
	   
	    There were 32 solutions found in 0.0944 seconds (with a Bezout bound of 32).
	    
	    Reference: "Computation of all solutions to a system of polynomial equations" by M. Kojima and S. Mizuno (pages 131-157),
	    "Finding all solutions to a system of polynomial equations" by A.H. Wright (pages 125-133), and "A simple homotopy method for determining all isolated solutions to
 	    polynomial systems" by W. Zulehner (pages 167-177).
	        
	    See also: http://homepages.math.uic.edu/~jan/Demo/wright.html.
	Example
    	    wright(QQ)
   ///