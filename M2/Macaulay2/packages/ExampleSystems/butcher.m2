export {"butcher"}

butcher = method()
butcher(Ring) := kk -> (
    (x,y,z,t,u,v,w) := (symbol x, symbol y, symbol z,
	symbol t, symbol u, symbol v, symbol w);
    R := kk[z,y,t,u,v,w,x];
    { z*u+y*v+t*w-w^2-1/2*w-1/2,
      z*u^2+y*v^2-t*w^2+w^3+w^2-1/3*t+4/3*w,
      x*z*v-t*w^2+w^3-1/2*t*w+w^2-1/6*t+2/3*w,
      z*u^3+y*v^3+t*w^3-w^4-3/2*w^3+t*w-5/2*w^2-1/4*w-1/4,
      x*z*u*v+t*w^3-w^4+1/2*t*w^2-3/2*w^3+1/2*t*w-7/4*w^2-3/8*w-1/8,
      x*z*v^2+t*w^3-w^4+t*w^2-3/2*w^3+2/3*t*w-7/6*w^2-1/12*w-1/12,
      -t*w^3+w^4-t*w^2+3/2*w^3-1/3*t*w+13/12*w^2+7/24*w+1/24
       }   			
)

beginDocumentation()

doc /// 
    Key
    	butcher
	(butcher,Ring)
    Headline
    	Butcher's problem
    Usage
    	butcher(kk)
    Inputs
    	kk:Ring
	    the coefficient ring
    Outputs
    	:List	
	    a list of the polynomials in this example
    Description
    	Text
	    This system was solved in May 2020, using @TO solveSystem@ in Macaulay2 v1.15
	     with an Intel(R) Core(TM) i5-4258U CPU at 2.40GHz.
	   
	    There were 368 solutions found in 82.589 seconds (with a Bezout bound of 4608).
	    
	    References: 
	    
	    W. Boege, R. Gebauer, and H. Kredel:
	    "Some examples for solving systems of algebraic equations by calculating Groebner bases", 
	    J. Symbolic Computation, 2:83-98, 1986.
	    
	    C. Butcher: "An application of the Runge-Kutta space".
 	    BIT, 24, pages 425--440, 1984.
	    
	    This example is from the POSSO test suite available by anonymous ftp from the site gauss.dm.unipi.it, from the directory pub/posso.
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/butcher.html
	Example
	    butcher(QQ)
    ///