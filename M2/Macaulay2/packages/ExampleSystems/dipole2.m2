export {"dipole2"}

dipole2 = method()
dipole2(Ring) := kk -> (
    (a,b,c,d,t,u,v,w) := (symbol a, symbol b, symbol c, symbol d, symbol t, symbol u, symbol v, symbol w);
    R := kk[a,b,c,d,t,u,v,w];
    {a+b-0.63254,
	c+d+1.34534,
	t*a+u*b-v*c-w*d+0.8365348,
	v*a+w*b+t*c+u*d-1.7345334,
	a*t^2-a*v^2-2*c*t*v+b*u^2-b*w^2-2*d*u*w-1.352352,
	c*t^2-c*v^2+2*a*t*v+d*u^2-d*w^2+2*b*u*w+0.843453,
	a*t^3-3*a*t*v^2+c*v^3-3*c*v*t^2+b*u^3-3*b*u*w^2+d*w^3-3*d*w*u^2+0.9563453,
	c*t^3-3*c*t*v^2-a*v^3+3*a*v*t^2+d*u^3-3*d*u*w^2-b*w^3+3*b*w*u^2-1.2342523
      }
  )

beginDocumentation()

doc /// 
    Key
    	dipole2
	(dipole2,Ring)
    Headline
    	Dipole2 system
    Usage
    	dipole2(kk)
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
	   
	    There were 4 solutions found in 22.0781 seconds (with a Bezout bound of 576).
	    
	    Reference: 
	    COCONUT, modified by COPRIN.
	    
	    See also: http://www-sop.inria.fr/coprin/logiciels/ALIAS/Benches/node1.html#SECTION000114000000000000000
	Example
	    dipole2(RR_53)
    ///