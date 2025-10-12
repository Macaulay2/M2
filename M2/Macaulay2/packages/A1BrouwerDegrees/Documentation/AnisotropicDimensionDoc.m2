doc ///
    Key
        getAnisotropicDimensionQQp
        (getAnisotropicDimensionQQp, GrothendieckWittClass, ZZ)
    Headline
	    returns the anisotropic dimension of a rational symmetric bilinear form over the p-adic rational numbers
    Usage
	    getAnisotropicDimensionQQp(beta, p)
    Inputs
        beta: GrothendieckWittClass
            over $\mathbb{Q}$
        p: ZZ
	        a prime number
    Outputs
	    : ZZ
	       the rank of the anisotropic part of $\beta$ over $\mathbb{Q}_{p}$
    Description
        Text
            This is an implementation of [KC18, Algorithm 8], which computes the anisotropic dimension of rational forms over the $p$-adic rational numbers. Any form of rank $\ge 5$ is isotropic, so this method will always return 0, 1, 2, 3, or 4.
    References
	    [KC18] P. Koprowski, A. Czogala, "Computing with quadratic forms over number fields," @ITALIC("Journal of Symbolic Computation")@, 2018.
    SeeAlso
	    getAnisotropicDimension
///

doc ///
    Key
        getAnisotropicDimension
        (getAnisotropicDimension, GrothendieckWittClass)
	(getAnisotropicDimension,  Matrix)
    Headline
        returns the anisotropic dimension of a symmetric bilinear form
    Usage
        getAnisotropicDimension beta
    Inputs
        beta: GrothendieckWittClass
               a GrothendieckWittClass over a field $k$, where $k$ is $\mathbb{Q}$, $\mathbb{R}$, $\mathbb{C}$, or a finite field of characteristic not 2 
    Outputs
            : ZZ
               the rank of the anisotropic part of $\beta$
    Description
        Text
            By the Witt Decomposition Theorem, any non-degenerate form decomposes uniquely as $\beta \cong n \mathbb{H} \oplus \beta_a$ where the form $\beta_a$ is anisotropic. The rank of $\beta_a$ is called the anisotropic dimension of $\beta$.
	        
            The anisotropic dimension of a form defined over the rational numbers is the maximum of the @TO2(getAnisotropicDimensionQQp, "getAnisotropicDimensionQQp")@ anistropic dimension at each of the completions of $\mathbb{Q}$ at the @TO2(getRelevantPrimes, "relevant primes")@.
    References
        [KC18] P. Koprowski, A. Czogala, "Computing with quadratic forms over number fields," @ITALIC("Journal of Symbolic Computation")@, 2018.
    SeeAlso
        getWittIndex
        getAnisotropicDimensionQQp
	        getAnisotropicPart
///

doc ///
    Key
        getWittIndex
        (getWittIndex, GrothendieckWittClass)
    Headline
        returns the Witt index of a symmetric bilinear form
    Usage
        getWittIndex beta
    Inputs
        beta: GrothendieckWittClass
               a GrothendieckWittClass denoted by $\beta\in\text{GW}(k)$, where $k$ is $\mathbb{Q}$, $\mathbb{R}$, $\mathbb{C}$, or a finite field of characteristic not 2
    Outputs
            : ZZ
               the rank of the totally isotropic part of $\beta$
    Description
        Text
	    By the Witt Decomposition Theorem, any non-degenerate form decomposes uniquely as $\beta \cong n \mathbb{H} \oplus \beta_a$ where the form $\beta_a$ is anisotropic. The integer $n$ is called the Witt index of $\beta$. See for instance [L05, I.4.3].
    References
	    [L05] T.Y. Lam, "Introduction to quadratic forms over fields," @ITALIC("American Mathematical Society")@, 2005.
    SeeAlso
        getAnisotropicDimension
///
