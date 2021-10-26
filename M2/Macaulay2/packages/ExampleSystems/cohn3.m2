export {"cohn3"}

cohn3 = method();
cohn3(Ring) := kk -> (
    (x,y,z,t) := (symbol x, symbol y, symbol z, symbol t);
    R := kk[x,y,z,t];
    { -x^3*y^2+2*x^2*y^2*z-x^2*y*z^2-144*x^2*y^2-207*x^2*y*z+288*x*y^2*z+78*x*y*z^2+x*z^3-3456*x^2*y-5184*x*y^2-9504*x*y*z-432*x*z^2-248832*x*y+62208*x*z- 2985984*x,
      -x^3*z*t^2+x^2*z^2*t^2-6*x^3*z*t+4*x^2*z^2*t+32*x^3*t^2-72*x^2*z*t^2-87*x*z^2*t^2-z^3*t^2-8*x^3*z-432*x^2*z*t-414*x*z^2*t+2592*x*z*t^2+864*z^2*t^2-1728*x^2*z-20736*x*z*t+3456*z^2*t-186624*z*t^2- 124416*x*z-1492992*z*t-2985984*z,
      x^2*y*t^3-2*x*y^2*t^3+y^3*t^3+8*x^2*y*t^2-12*x*y^2*t^2+4*y^3*t^2-24*x*y*t^3+24*y^2*t^3+20*x^2*y*t-20*x*y^2*t-160*x*y*t^2+96*y^2*t^2+128*x*t^3+16*x^2*y+96*x*y*t+2304*x*t^2+1152*x*y+13824*x*t+27648*x,
      y^3*t^3-y^2*z*t^3+4*y^3*t^2-2*y^2*z*t^2+72*y^2*t^3+71*y*z*t^3+288*y^2*t^2+360*y*z*t^2+6*z^2*t^2+1728*y*t^3-464*z*t^3+432*y*z*t+8*z^2*t+ 6912*y*t^2-4320*z*t^2+13824*t^3+z^2-13824*z*t+55296*t^2-13824*z
      }
)

beginDocumentation()

doc /// 
    Key
    	cohn3
	(cohn3,Ring)
    Headline
    	modular equations for special algebraic number fields
    Usage
    	cohn3(kk)
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
	   
	    There were 72 solutions found in 13.8985 seconds (with a Bezout bound of 1080).
	    
	    Reference: 
	    See the PoSSo test suite.
	    Andre' Galligo and Carlo Traverso.
	    "Practical Determination of the dimension of an algebraic variety",
	    in E. Kaltofen and S.M. Watt, Eds "Computers and Mathematics", pages 46-52, 1989.
	    
	    H. Cohn.
	    "An explicit modular equation in two variables and Hilbert's Twelfth problem", 
	    Math. of Comp. 38, pp. 227-236, 1982.
	    
	    H. Cohn, J. Deutch.
	    "An explicit modular equation in two variables for Q[sqrt(3)]", 
	    Math. of Comp. 50, pp. 557-568, 1988.
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/cohn3.html
	Example
	    F = cohn3(QQ)
    ///
