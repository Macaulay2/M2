export { "cyclic" }

cyclic = method()
cyclic (ZZ,Ring) := (n,kk) -> (
    R := kk[vars(0..n-1)];
    toList apply(1..n-1, d-> sum(0..n-1, 
	    i -> product(d, k -> R_((i+k)%n))
	    )) | {product gens R - 1}
    )

doc /// 
    Key
    	cyclic
	(cyclic,ZZ,Ring)
    Headline
    	an example of a square polynomial system 
    Description
    	Text
	    {\bf Cyclic n-roots} is a popular benchmark problem. 
	    It has finitely many solutions iff n is square free. 
	    In this case the number of solutions is less than the Bezout bound.
	Example
	    F = cyclic(5,QQ)
	    sols = solveSystem F;
	    #sols
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
