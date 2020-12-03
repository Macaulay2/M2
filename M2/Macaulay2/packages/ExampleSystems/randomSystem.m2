export { "randomSystem" }

randomSystem = method()
randomSystem (ZZ,ZZ,Ring) := (n,d,kk) -> (
     R := kk[vars(53..n+52)];
     apply(n, i->sum(toList(1..d),j->random(j,R)) - 1)
     )

beginDocumentation()

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
	  positive value representing the number of polynomials
        d:ZZ
	  positive value representing the degree of the polynomials
	K:Ring
	  usually a field, e.g., QQ or CC_{53}
    Outputs
        F:List
	  a system of $n$ polynomials random polynomials of degree $d$ in $n$ variables.
    Description
    	Text
	    This system was solved in May 2020, using @TO track@ in Macaulay2 v1.15
	     with an Intel(R) Core(TM) i5-5250U CPU at 1.60GHz.
	   
	    For a system with 2 polynomials of degree 3 over rationals there were 9 solutions found in 0.06 seconds.
	    
	    For a system with 5 polynomials of degree 2 over complex numbers there were 32 solutions found in 0.297 seconds.
	Example
	    randomSystem(2,3,QQ)
	    randomSystem(5, 2, CC_53)
    ///

-* TEST ///
F = randomSystem(5,2,CC_53)
time sols = solveSystem F
assert(#sols==32)
/// *-