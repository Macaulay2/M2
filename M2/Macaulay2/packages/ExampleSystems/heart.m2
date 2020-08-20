export{"heart"}

heart = method()
heart (Ring) := kk -> (
    (a, b, c, d, t, u, v, w) := (symbol a, symbol b, symbol c, symbol d, symbol t, symbol u, symbol v, symbol w);
    R := kk[a,b,c,d,t,u,v,w];
   { a + b - 0.63254,
       c + d + 1.34534,
       t*a + u*b - v*c - w*d + 0.8365348,
       v*a + w*b + t*c + u*d - 1.7345334,
       a*t**2 - a*v**2 - 2*c*t*v + b*u**2 - b*w**2 - 2*d*u*w - 1.352352,
       c*t**2 - c*v**2 + 2*a*t*v + d*u**2 - d*w**2 + 2*b*u*w + 0.843453,
       a*t**3 - 3*a*t*v**2 + c*v**3 - 3*c*v*t**2 + b*u**3 - 3*b*u*w**2 + d*w**3 - 3*d*w*u**2 + 0.9563453,
       c*t**3 - 3*c*t*v**2 - a*v**3 + 3*a*v*t**2 + d*u**3 - 3*d*u*w**2 - b*w**3 + 3*b*w*u**2 - 1.2342523 }
 )

    beginDocumentation()

doc /// 
    Key
    	heart
	(heart,Ring)
    Headline
    	an 8-dimensional economics problem 
    Usage
    	heart(kk)
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
	   
	    There were 2 solutions found in 3.356 seconds (with a Bezout bound of 1458).
	    
	    Reference: "Determination of magnitudes, directions, and locations of two independent dipoles in a circular conducting region from boundary potential measurements" by C.V. Nelson and B.C. Hodgkin (pages 817-823),
	    "Coefficient-Parameter Polynomial Continuation" by A.P. Morgan and A.J. Sommese (pages 123-160), 
	    "Mathematical reduction of a heart dipole model" by A.P. Morgan and A. Sommese and L.T. Watson (pages 407-410).
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/heart.html.
	Example
	    heart(RR_53)
    ///
    