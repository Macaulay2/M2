export{"geneig"}

geneig = method()
geneig(Ring) := kk -> (
    x := symbol x;
    R := kk[x_1..x_6];
   { -10*x_1*x_6^2+ 2*x_2*x_6^2-x_3*x_6^2+x_4*x_6^2+ 3*x_5*x_6^2+x_1*x_6+ 2*x_2*x_6+x_3*x_6+ 2*x_4*
       x_6+x_5*x_6+ 10*x_1+ 2*x_2-x_3+ 2*x_4-2*x_5,
       2*x_1*x_6^2-11*x_2*x_6^2+ 2*x_3*x_6^2-2*x_4*x_6^2+x_5*x_6^2+ 2*x_1*x_6+x_2*x_6+ 2*x_3*x_6+x_4*
       x_6+ 3*x_5*x_6+ 2*x_1+ 9*x_2+ 3*x_3-x_4-2*x_5,
       -x_1*x_6^2+ 2*x_2*x_6^2-12*x_3*x_6^2-x_4*x_6^2+x_5*x_6^2+x_1*x_6+ 2*x_2*x_6-2*x_4*x_6-2*x_5*x_6-
       x_1+ 3*x_2+ 10*x_3+ 2*x_4-x_5,
       x_1*x_6^2-2*x_2*x_6^2-x_3*x_6^2-10*x_4*x_6^2+ 2*x_5*x_6^2+ 2*x_1*x_6+x_2*x_6-2*x_3*x_6+ 2*x_4*
       x_6+ 3*x_5*x_6+ 2*x_1-x_2+ 2*x_3+ 12*x_4+x_5,
       3*x_1*x_6^2+x_2*x_6^2+x_3*x_6^2+ 2*x_4*x_6^2-11*x_5*x_6^2+x_1*x_6+ 3*x_2*x_6-2*x_3*x_6+ 3*x_4*
       x_6+ 3*x_5*x_6-2*x_1-2*x_2-x_3+x_4+ 10*x_5,
       x_1+x_2+x_3+x_4+x_5-1 }
 )

  
  beginDocumentation()

doc /// 
    Key
    	geneig
	(geneig,Ring)
    Headline
    	generalized eigenvalue problem 
    Usage
    	geneig(kk)
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
	   
	    There were 10 solutions found in 1.447 seconds (with a Bezout bound of 243). 
	    
	    Reference: "Homotopy method for general lambda-matrix problems" by M. Chu, T.Y. Li and T. Sauer (pages 528-536).
	     
	    See also: http://homepages.math.uic.edu/~jan/Demo/geneig.html.
	Example
            geneig(QQ)
    ///