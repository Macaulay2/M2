export { "cyclic" }

cyclic = method()
cyclic (ZZ,Ring) := (n,kk) -> (
    R := kk[vars(0..n-1)];
    toList apply(1..n-1, d-> sum(0..n-1, 
	    i -> product(d, k -> R_((i+k)%n))
	    )) | {product gens R - 1}
    )

beginDocumentation()

doc /// 
    Key
    	cyclic
	(cyclic,ZZ,Ring)
    Headline
    	an example of a square polynomial system
    Usage
    	cyclic(n,kk)
    Inputs
    	n:ZZ
	    	the number of variables
	kk:Ring
	    	the coefficient ring
    Outputs
    	:List
	    	of polynomials in the system
    Description
    	Text
	    {\bf Cyclic n-roots} is a popular benchmark problem. 
	    It has finitely many solutions iff n is square free. 
	    In this case the number of solutions is less than the Bezout bound.
	    
	    This system was solved in May 2020, using @TO solveSystem@ in Macaulay2 v1.15
	     with an Intel(R) Core(TM) i5-5250U CPU at 1.60GHz.
	   
	    There were 70 solutions found in 2.737 seconds with 5 variables.
	Example
	    cyclic(5,QQ)
    ///

-- test disabled, because it will fail on slightly slower machines

-- TEST ///
-- F = cyclic(5,CC_53)
-- assert(
--     first timing (sols = solveSystem F;) 
--     < 3
--     )
-- assert(#sols==70)
-- ///
