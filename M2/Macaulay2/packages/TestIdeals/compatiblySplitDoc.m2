doc ///
    Key
        compatibleIdeals
        (compatibleIdeals, RingElement)
        [compatibleIdeals, FrobeniusRootStrategy]
    Headline
        finds all ideals compatibly compatible with a Frobenius near-splitting ideals
    Usage
        compatibleIdeals (u)
    Inputs
        u:RingElement
            the element determining the Frobenius splitting
        FrobeniusRootStrategy => Symbol
            choose the strategy for internal frobeniusRoot calls
    Outputs
        :List
    Description
        Text
            The given an element $u$ in a polynomial ring $R$ over a prime field defines a
	    $p^{-e}$ linear map $\phi$: this is obtained by multiplying $e$-th Frobenius trace on a polynomial ring by the polynomial $u$.
	    An ideal $I$ is $\phi$-compatible if $\phi(I)\subseteq I$ or, equivalently, $u I \subseteq I^{[p]}$.
	    This function returns a list of all prime ideals $P$ such that:

            (a) $u P \subseteq P^{[p]}$, and

            (b) $u$ is not in $P^{[p]}$.

            Condition (b) is equivalent to the non-vanishing of the corresponding near-splitting of $R/P$. When $\phi$ is surjective, the set of all $\phi$-compatible ideals consists of all intersections of the
	    primes above.

	    This function is an implementation of the algorithm described in Moty Katzman and Karl Schwede's paper "An algorithm for computing compatibly Frobenius split subvarieties" J. Symbolic Comput. 47 (2012), no. 8, 996-1008.

	    These prime ideals have a "Matlis-dual" interpretation, too. Let $E$ be the injective hull of the residue field of the localization or $R$ at the irrelevant ideal,
	    and let $T: E \rightarrow E$ be the natural Frobenius map. Then $uT$ is a Frobenius map on $E$, and the primes $P$ computed by this function are precisely those for which
	    $uT$ restricts to a non-zero Frobenius map of the annihlator of $P$ on $E$.

            We begin with a simple example (what is split with the coordinate axes in A^2).
        Example
            R = ZZ/3[u,v];
            u = u^2*v^2;
            compatibleIdeals(u)
        Text
            Here is a more substantial example.
        Example
            R=ZZ/2[x_{21},x_{31},x_{32},x_{41},x_{42},x_{43}];
            u=x_{41}*(x_{31}*x_{42}-x_{41}*x_{32})*(x_{41}-x_{21}*x_{42}-x_{31}*x_{43}+x_{21}*x_{32}*x_{43});
            C=compatibleIdeals (u);
            apply(C, print);
        Text
            The option {\tt FrobeniusRootStrategy} is passed to internal @TO frobeniusRoot@ calls.
///
