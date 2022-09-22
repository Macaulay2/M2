export { "randomGeneralizedEigenvalueProblem" }

randomGeneralizedEigenvalueProblem = method()
randomGeneralizedEigenvalueProblem ZZ := n -> (
     lambda := symbol lambda;
     R := CC[lambda, vars(53..n+52)];
     A := sub(random(CC^n,CC^n), R);
     B := sub(random(CC^n,CC^n), R);
     x := transpose matrix{drop(gens R,1)};
     T := flatten entries (A*x-R_0*B*x) | {n - 1 - sum flatten entries x};
     S := apply(n,i->(R_0-exp(ii*2*pi*(i/n)))*(x_(i,0)-1)) | {n - 1 - sum flatten entries x};
     solsS := apply(n,i->point{{exp(ii*2*pi*(i/n))} | toList(i:1) | {0} | toList(n-i-1:1)});
     (T,S,solsS)
     )

beginDocumentation()

doc ///
    Key
    	randomGeneralizedEigenvalueProblem
	(randomGeneralizedEigenvalueProblem,ZZ)
    Headline
    	an example of a 0-dimensional square polynomial system
    Usage
        (T,S,solsS) = randomGeneralizedEigenvalueProblem(n,K)
    Inputs
        n:ZZ
	    positive value representing the number of polynomials
    Outputs
        T'S'solsS:Sequence
	            target system T, start system S, and solutions solsS to the start system
    Description
        Text
	    Given $n\times n$ matrices $A$ and $B$, 
	    a number $\lambda$ is a {\bf generalized eigenvalue}
	    if there is a nonzero vecor $v$ such that $A x = \lambda B x$.
	Text
	    This function creates a square target system representing the problem
	    for random $A$ and $B$ and a start system representing  
	    the problem with eigenvalues that
	    are $n$-th roots of unity and the corresponding eigenvectors 
	    form the standard basis. 
	    
	    This system was solved in May 2020, using @TO track@ in Macaulay2 v1.15
	     with an Intel(R) Core(TM) i5-5250U CPU at 1.60GHz.
	     
	     For a system of 3 polynomials there were 3 solutions found in 0.0764 seconds.
	     
	     For a system of 5 polynomials there were 5 solutions found in 0.0478 seconds.
	Example
	    randomGeneralizedEigenvalueProblem 3
	    randomGeneralizedEigenvalueProblem 5
    ///

-* TEST ///
F = first randomGeneralizedEigenvalueProblem 5
sols = solveSystem F
assert(#sols==5)
/// *-
