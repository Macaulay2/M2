export {"camera1s"}

camera1s = method();
camera1s(Ring) := kk -> (
    (d,q) := (symbol d, symbol q);
    R := kk[d_1..d_3,q_1..q_3];
    { - d_1*q_1 - d_2*q_2 - d_3*q_3 + 1,
      - 3.6*d_1*q_1 + 4.1*d_1*q_2 + 2.0*d_1*q_3 + 0.1*d_1 + 4.1*d_2*q_1 + 1.8*d_2*q_2 + 3.7*d_2*q_3 - 0.2*d_2 + 2.0*d_3*q_1 + 3.7*d_3*q_2 - 4.0*d_3*q_3 + 0.3*d_3 + 0.1*q_1 - 0.2*q_2 + 0.3*q_3 + 5.8,
      - 2.140796*d_1*q_1 - 3.998792*d_1*q_2 + 3.715992*d_1*q_3 - 0.2828*d_1 - 3.998792*d_2*q_1 - 1.575196*d_2*q_2 - 3.998792*d_2*q_3 + 3.715992*d_3*q_1 - 3.998792*d_3*q_2 - 2.140796*d_3*q_3 + 0.2828*d_3 - 0.2828*q_1 + 0.2828*q_3 + 5.856788,
      0.3464*d_1*q_1 + 0.1732*d_1*q_2 - 5.999648*d_1*q_3 - 0.1732*d_1 + 0.1732*d_2*q_1 - 5.999648*d_2*q_2 - 0.1732*d_2*q_3 + 0.3464*d_2 - 5.999648*d_3*q_1 - 0.1732*d_3*q_2 - 0.3464*d_3*q_3 - 0.1732*d_3 - 0.1732*q_1 + 0.3464*q_2 - 0.1732*q_3 + 5.999648,
      - 5701.3*d_1*q_1 - 2.9*d_1*q_2 + 3796.7*d_1*q_3 - 1902.7*d_1 - 2.9*d_2*q_1 - 5698.7*d_2*q_2 + 1897.3*d_2*q_3 + 3803.3*d_2 + 3796.7*d_3*q_1 + 1897.3*d_3*q_2 + 5703.1*d_3*q_3 + 0.7*d_3 - 1902.7*q_1 + 3803.3*q_2 + 0.7*q_3 + 5696.9,
      - 6.8*d_1*q_1 - 3.2*d_1*q_2 + 1.3*d_1*q_3 + 5.1*d_1 - 3.2*d_2*q_1 - 4.8*d_2*q_2 - 0.7*d_2*q_3 - 7.1*d_2 + 1.3*d_3*q_1 - 0.7*d_3*q_2 + 9.0*d_3*q_3 - d_3 + 5.1*q_1 - 7.1*q_2 - q_3 + 2.6
   }
)

beginDocumentation()

doc /// 
    Key
    	camera1s
	(camera1s,Ring)
    Headline
    	a system for camera displacement between two positions, with scaled first frame.
    Usage
    	camera1s(kk)
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
	   
	    There were 20 solutions found in 0.2575 seconds (with a Bezout bound of 64).
	    
	    Reference: 
	    
	    Ioannis Z. Emiris.
	    "Sparse Elimination and Application in Kinematics".
	    PhD Thesis, Computer Science, University of California at Berkeley, 1994.	 
    	    
	    Ioannis Z. Emiris.
	    "A general Solver Based on Sparse Resultants:
 	    Numerical Issues and Kinematic Applications",
	    INRIA Rapport de Recherche no 3110, January 1997, 29 pages.
	    Available via anonymous ftp to ftp.inria.fr
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/camera1s.html
	Example
	    camera1s(RR_101)
    ///
