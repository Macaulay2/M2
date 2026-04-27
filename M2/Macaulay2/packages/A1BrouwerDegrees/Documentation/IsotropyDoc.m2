doc ///
    Key
        isIsotropic
        (isIsotropic, GrothendieckWittClass)
        (isIsotropic, Matrix)
    Headline
        determines whether a Grothendieck-Witt class is isotropic
    Usage
        isIsotropic beta
    Inputs
        beta: GrothendieckWittClass
            denoted by $\beta\in\text{GW}(k)$, where $k$ is $\mathbb{C}$, $\mathbb{R}$, $\mathbb{Q}$, or a finite field of characteristic not 2
    Outputs
        : Boolean
            whether $\beta$ is isotropic
    Description
        Text
            This is the negation of the boolean-valued @TO2(isAnisotropic, "isAnisotropic")@. See documentation there.
    SeeAlso
        isAnisotropic
        getWittIndex
        getAnisotropicDimension
///

doc ///
    Key
        isAnisotropic
        (isAnisotropic, GrothendieckWittClass)
        (isAnisotropic, Matrix)
    Headline
        determines whether a Grothendieck-Witt class is anisotropic
    Usage
        isAnisotropic beta
    Inputs
        beta: GrothendieckWittClass
            denoted by $\beta\in\text{GW}(k)$, where $k$ is $\mathbb{C}$, $\mathbb{R}$, $\mathbb{Q}$, or a finite field of characteristic not 2
    Outputs
        : Boolean
            whether $\beta$ is anisotropic
    Description
        Text
            Recall a symmetric bilinear form $\beta$ is said to be @ITALIC("isotropic")@ if there exists a nonzero vector $v$ for which $\beta(v,v) = 0$. Witt's decomposition theorem implies that a non-degenerate symmetric bilinear form decomposes uniquely into an isotropic and an anisotropic part. Certifying (an)isotropy is then an important computational problem when working with the Grothendieck-Witt ring.

            Over $\mathbb{C}$, any form of rank two or higher contains a copy of the hyperbolic form, and hence is isotropic. Thus we can determine anisotropy simply by a consideration of rank.
        Example
            isAnisotropic makeGWClass matrix(CC, {{3}})
            isAnisotropic makeGWClass matrix(CC, {{2,0},{0,5}})
        Text
            Forms over $\mathbb{R}$ are anisotropic if and only if all its diagonal entries are positive or are negative.
        Example
            isAnisotropic makeGWClass matrix(RR, {{3,0,0},{0,5,0},{0,0,7}})
            isAnisotropic makeGWClass matrix(RR, {{0,2},{2,0}})
        Text
            Over $\mathbb{Q}$, things become a bit more complicated. We can exploit the local-to-global principle for isotropy (the @ITALIC("Hasse-Minkowski principle")@), which states that a form is isotropic over $\mathbb{Q}$ if and only if it is isotropic over all its completions, meaning all the $p$-adic numbers and $\mathbb{R}$ [L05, VI.3.1]. We note, however, the classical result that all forms of rank $\ge 5$ in $\mathbb{Q}_p$ are isotropic [S73, IV Theorem 6]. Thus isotropy in this range of ranks is equivalent to checking it over the real numbers.
        Example
            beta = makeGWClass matrix(QQ, {{1,0,2,0,3},{0,6,1,1,-1},{2,1,5,2,0},{0,1,2,4,-1},{3,-1,0,-1,1}});
            isAnisotropic beta
            getDiagonalClass beta
        Text
            For forms of rank $\le 4$, the problem reduces to computing the maximum anisotropic dimension of the form over local fields. Ternary forms are isotropic away from primes dividing the coefficients of the form in a diagonal basis by e.g. [L05, VI.2.5(2)], so there are only finitely many places to check. Over these @TO2(getRelevantPrimes, "relevant primes")@, isotropy of a form $\beta \in \text{GW}(\mathbb{Q})$ over $\mathbb{Q}_p$ is equivalent to the statement that $(-1,-\text{disc}(\beta))_p = H(\beta)$, where $H(\beta)$ denotes the @TO2(getHasseWittInvariant, "Hasse-Witt invariant")@ attached to $\beta$ and $(-,-)_p$ is the @TO2(getHilbertSymbol, "Hilbert Symbol")@.

            Over finite fields, a form is anisotropic so long as it is nondegenerate, of rank $\le 2$ and not isomorphic to the hyperbolic form.
        Example
            isAnisotropic makeGWClass matrix(GF(7), {{1,0,0},{0,1,0},{0,0,1}})
            isAnisotropic makeGWClass matrix(GF(7), {{3,0},{0,3}})
        Text
            A binary form $q$ is isotropic if and only if it is isomorphic to the hyperbolic form, which implies in particular that the rank, @TO2(getSignature, "signature")@, and @TO2(getIntegralDiscriminant, "discriminant")@ of $q$ agree with that of $\mathbb{H}=\langle 1,-1\rangle$.
    References
        [L05] T.Y. Lam, "Introduction to quadratic forms over fields," American Mathematical Society, 2005.

        [S73] J.P. Serre, "A course in arithmetic," Springer-Verlag, 1973.
    SeeAlso
        isIsotropic
        getAnisotropicDimension
        getAnisotropicPart
        getWittIndex
///