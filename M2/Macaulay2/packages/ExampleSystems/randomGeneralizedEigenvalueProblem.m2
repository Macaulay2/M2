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
	  positive
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
	    are $n$-th roots of unity and the corresponding eignevectors 
	    form the standard basis. 
	Example
	    (T,S,solsS) = randomGeneralizedEigenvalueProblem 3
	    solsT = track(S,T,solsS)
	    #solsT
    ///

TEST ///
F = first randomGeneralizedEigenvalueProblem 5
assert(
    first timing (sols = solveSystem F;)
    < 1
    )
assert(#sols==5)
///
