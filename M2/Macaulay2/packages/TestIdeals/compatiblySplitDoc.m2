--***********************************************
--***********************************************
--Documentation for compatiblySplit.m2
--***********************************************
--***********************************************

doc ///
    Key
        compatibleIdeals
        (compatibleIdeals, RingElement)
        [compatibleIdeals, FrobeniusRootStrategy]
    Headline
        find all prime ideals compatible with a Frobenius near-splitting 
    Usage
        compatibleIdeals(u)
    Inputs
        u:RingElement
            in a polynomial ring over the prime field $\mathbb{Z}/p$; the element determining the Frobenius splitting
        FrobeniusRootStrategy => Symbol
            selects the strategy for internal {\tt frobeniusRoot} calls
    Outputs
        :List
	    containing all prime ideals {\tt P} of the ring of {\tt u} such that {\tt uP} $\subseteq$ {\tt P^{[p]}} and {\tt u} is not in {\tt P^{[p]}}

    Description
        Text
            The given element $u$ in a polynomial ring $R$ over the prime field $\mathbb{Z}/p$ defines a $p^{-1}$-linear map $\phi: R\to R$; this map consists of multiplication by the polynomial $u$, followed by the Frobenius trace on the polynomial ring $R$.
	    An ideal $I$ of $R$ is $\phi$-compatible if $\phi(I)\subseteq I$ or, equivalently, $u I \subseteq I^{[p]}$.
	    The function {\tt compatibleIdeals} returns a list of all prime ideals $P$ of $R$ such that:

            (a) $uP \subseteq P^{[p]}$, and

            (b) $u$ is not in $P^{[p]}$.

            Condition (b) is equivalent to the non-vanishing of the corresponding near-splitting of $R/P$. 
	    When $\phi$ is surjective, the set of all $\phi$-compatible ideals consists of all intersections of the above prime ideals.

	    This function is an implementation of the algorithm described in Moty Katzman and Karl Schwede's paper {\it An algorithm for computing compatibly Frobenius split subvarieties}, J. Symbolic Comput. 47 (2012), no. 8, pp. 996-1008.

	    These prime ideals have a "Matlis-dual" interpretation, too. 
	    Let $E$ be the injective hull of the residue field of the localization of $R$ at the irrelevant ideal, and let $T: E \rightarrow E$ be the natural Frobenius map. 
	    Then $uT$ is a Frobenius map on $E$, and the primes $P$ computed by this function are precisely those for which $uT$ restricts to a nonzero Frobenius map of the annihilator of $P$ on $E$.

            The following is a simple example, which is split with the coordinate axes in $\mathbb{A}^2$.
        Example
            R = ZZ/3[s,t];
            u = s^2*t^2;
            compatibleIdeals u
        Text
            Here is a more substantial example.
        Example
            R = ZZ/2[a,b,c,d,e,f];
            u = d*(b*e - d*c)*(d - a*e - b*f + a*c*f);
            print \ compatibleIdeals u;
        Text
            The option {\tt FrobeniusRootStrategy} is passed to internal @TO frobeniusRoot@ calls.
///
