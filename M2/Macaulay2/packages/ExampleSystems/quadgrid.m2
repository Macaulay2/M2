export{"quadgrid"}

quadgrid = method()
quadgrid (Ring) := kk -> (
	(w,b) := (symbol w, symbol b);
	R := kk[w_0..w_3,b];
	{ w_0 + w_1 + w_2 + w_3 - 1,
		w_0*b + w_1*b + w_2*b + w_3*b + 1/2*w_1 + w_2 + 3/2*w_3 - 0.63397459621556,
		w_0*b^2 + w_1*b^2 + w_2*b^2 + w_3*b^2 + w_1*b + 2*w_2*b + 3*w_3*b + 1/4*w_1 + w_2 + 9/2*w_3 - 0.40192378864668,
		w_0*b^3 + w_1*b^3 + w_2*b^3 + w_3*b^3 + 3/2*w_1*b^2 + 3*w_2*b^2 + 9/2 *w_3*b^2 + 3/4*w_1*b + 3*w_2*b + 27/4*w_3*b + 1/8*w_1 + w_2 + 27/8*w_3 - 0.13109155679036,
		w_0*b^4 + w_1*b^4 + w_2*b^4 + w_3*b^4 + 2 * w_1*b^3 + 4*w_2*b^3 + 6 * w_3*b^3 + 3/2* w_1*b^2 + 6*w_2*b^2 + 27/2* w_3*b^2 + 1/2* w_1*b + 4*w_2*b + 27/2* w_3*b + 1/16*w_1 + w_2 + 81/16*w_3 + 0.30219332850656 }
 )

beginDocumentation()

doc /// 
    Key
    	quadgrid
	(quadgrid,Ring)
    Headline
    	interpolating quadrature formula for function defined on a grid
    Usage
	quadgrid(kk)
    Inputs
	kk:Ring
		the coefficient ring
    Outputs
	:List
		of polynomials in the system
    Description
    	Text
	    This system was solved in May 2020, using @TO solveSystem@ in Macaulay2 v1.15
	     with an Intel(R) Core(TM) i5-5250U CPU at 1.60GHz.
	   
	    There were 5 solutions found in 1.653 seconds (with a Bezout bound of 120).
	    
	    Note: This system is ill-conditioned. There are 4 complex and 1 real solution.
	    
	    Reference: "The construction and application of wavelets in numerical analysis" by Wim Sweldens.
	    
	    See also: http://homepages.math.uic.edu/~jan/Demo/quadgrid.html.
	Example
	    quadgrid(RR_53)
    ///