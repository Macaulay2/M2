export { "randomSystem" }

randomSystem = method()
randomSystem (ZZ,ZZ,Ring) := (n,d,kk) -> (
     R := kk[vars(53..n+52)];
     apply(n, i->sum(toList(1..d),j->random(j,R)) - 1)
     )

doc ///
    Key
    	randomSystem
	(randomSystem,ZZ,ZZ,Ring)
    Headline
    	an example of a 0-dimensional square polynomial system
    Usage
        F = randomSystem(n,d,K)
    Inputs
        n:ZZ
	  positive
        d:ZZ
	  positive
	K:Ring
	  usually a field, e.g., QQ or CC_53
    Outputs
        F:List
	  a system of $n$ polynomials random polynomials of degree $d$ in $n$ variables.
    Description
	Example
	    F = randomSystem(2,3,QQ)
	    sols = solveSystem F;
	    #sols
    ///

TEST ///
F = randomSystem(5,2,CC_53)
assert(
    first timing (sols = solveSystem F;)
    < 1
    )
assert(#sols==32)
///
