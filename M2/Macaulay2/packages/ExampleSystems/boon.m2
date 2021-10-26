export {"boon"}

boon = method()
boon(Ring) := kk -> (
    (s,g,C) := (symbol s, symbol g, symbol C);
    R := kk[s_1,s_2,g_1,g_2,C_1,C_2];
    {s_1^2 + g_1^2 - 1,
      s_2^2 + g_2^2 - 1,
      C_1*g_1^3 + C_2*g_2^3 - 6/5,
      C_1*s_1^3 + C_2*s_2^3 - 6/5,
      C_1*g_1^2*s_1 + C_2*g_2^2*s_2 - 7/10,
      C_1*g_1*s_1^2 + C_2*g_2*s_2^2 - 7/10
      }
  )

beginDocumentation()

doc /// 
    Key
    	boon
	(boon,Ring)
    Headline
    	a neurophysiology problem posted by Sjirk Boon
    Usage
    	boon(kk)
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
	   
	    There were 8 solutions found in 10.1037 seconds (with a Bezout bound of 1024).
	    
	    Reference: 
	    P. Van Hentenryck, D. McAllester and D. Kapur:
	    "Solving Polynomial Systems Using a Branch and Prune Approach"
	    SIAM J. Numerical Analysis, Vol. 34, No. 2, pp 797-827, 1997.
	    
	    See also: post to sci.math.num-analysis by Sjirk Boon
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/boon.html	    
	Example
	    boon(QQ)
    ///