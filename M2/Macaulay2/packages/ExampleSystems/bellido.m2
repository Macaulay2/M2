export {"bellido"}

bellido = method()
bellido(Ring) := kk -> (
    z := symbol z;
    R := kk[z_1..z_9];
    {(z_1-6)^2+z_2^2+z_3^2-104,
	z_4^2+(z_5-6)^2+z_6^2-104,
	z_7^2+(z_8-12)^2+(z_9-6)^2-80,
	z_1*(z_4-6)+z_5*(z_2-6)+z_3*z_6-52,
	z_1*(z_7-6)+z_8*(z_2-12)+z_9*(z_3-6)+64,
	z_4*z_7+z_8*(z_5-12)+z_9*(z_6-6)-6*z_5+32,
	2*z_2+2*z_3-2*z_6-z_4-z_5-z_7-z_9+18,
	z_1+z_2+2*z_3+2*z_4+2*z_6-2*z_7+z_8-z_9-38,
	z_1+z_3+z_5-z_6+2*z_7-2*z_8-2*z_4+8
      }
  )

beginDocumentation()

doc /// 
    Key
    	bellido
	(bellido,Ring)
    Headline
    	Bellido system
    Usage
    	bellido(kk)
    Inputs
    	kk:Ring
	    the coefficient ring
    Outputs
    	:List	
	    of the polynomials in the system
    Description
    	Text
	    This system was solved in May 2020, using @TO solveSystem@ in Macaulay2 v1.15
	     with an Intel(R) Core(TM) i5-8250U CPU at 1.60GHz.
	   
	    There were 40 solutions found in 0.546875 seconds (with a Bezout bound of 64).
	    
	    Reference: 
	    Bellido A.M.
	    "Construction of iteration functions for the simultaneous computation of the solutions of equations and algebraic systems."
	    Numerical Algorithms, 6(3-4):315-351, 1992.
	    
	    See also: http://www-sop.inria.fr/coprin/logiciels/ALIAS/Benches/node1.html#SECTION00012000000000000000
	Example
	    bellido(QQ)
    ///