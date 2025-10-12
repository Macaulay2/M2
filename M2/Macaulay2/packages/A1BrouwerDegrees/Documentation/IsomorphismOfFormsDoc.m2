doc ///
    Key
        isIsomorphicForm
        (isIsomorphicForm, GrothendieckWittClass, GrothendieckWittClass)
        (isIsomorphicForm, UnstableGrothendieckWittClass, UnstableGrothendieckWittClass)
        (isIsomorphicForm, Matrix, Matrix)
        [isIsomorphicForm, linearTolerance]
    Headline
        determines whether two (unstable) Grothendieck-Witt classes over $\mathbb{C},\mathbb{R},\mathbb{Q}$ or a finite field of characteristic not 2 are isomorphic.
    Usage
        isIsomorphicForm(alpha, beta)
        isIsomorphicForm(M, N)
    Inputs
        alpha : GrothendieckWittClass
            denoted by $\alpha$ over $\mathbb{C},\mathbb{R},\mathbb{Q}$ or a finite field of characteristic not 2
        beta : GrothendieckWittClass
            denoted by $\beta$ over $\mathbb{C},\mathbb{R},\mathbb{Q}$ or a finite field of characteristic not 2
        alpha : UnstableGrothendieckWittClass
            denoted by $\alpha$ over $\mathbb{C},\mathbb{R},\mathbb{Q}$ or a finite field of characteristic not 2
        beta : UnstableGrothendieckWittClass
            denoted by $\beta$ over $\mathbb{C},\mathbb{R},\mathbb{Q}$ or a finite field of characteristic not 2
        M : Matrix
            a symmetric matrix of full rank denoted by @TT("M")@ over $\mathbb{C},\mathbb{R},\mathbb{Q}$ or a finite field of characteristic not 2
        N : Matrix
            a symmetric matrix of full rank denoted by @TT("N")@ over $\mathbb{C},\mathbb{R},\mathbb{Q}$ or a finite field of characteristic not 2
        linearTolerance => RR
            a positive number specifying the tolerance to which the $k^{\times}$-factors of unstable Grothendieck-Witt classes over $\mathbb{R}$ or $\mathbb{C}$ are considered equal 
    Outputs
        : Boolean
            whether the two Grothendieck-Witt classes (resp. unstable Grothendieck-Witt classes) are equal as elements of the Grothendieck-Witt ring (resp. unstable Grothendieck-Witt group)
    Description
        Text
            Determining if Grothendieck-Witt classes (resp. unstable Grothendieck-Witt classes) $\alpha$ and $\beta$ over a field $k$ are isomorphic is to verify when forms are isomorphic in the Grothendieck-Witt ring (resp. in the unstable Grothendieck-Witt group). In the case of the unstable Grothendieck-Witt group, the structure of the group $\text{GW}^{u}(k)$ as a fibered product $\text{GW}(k)\times_{k^{\times}/(k^{\times})^{2}}k^{\times}$ implies that it suffices to verify that their $\text{GW}(k)$-factors are isomorphic and their $k^{\times}$-factors are equal. 

            @ITALIC("Sylvester's Law of Inertia")@ proves that any symmetric bilinear form can be diagonalized into a block sum of rank one symmetric bilinear forms. Since the rank one forms $\langle a\rangle:k\times k\to k, (x,y)\mapsto axy$ and $\langle ab^{2}\rangle:k\times k\to k, (x,y)\mapsto ab^{2}xy$ differ by a change of basis in the ground field, it follows that they are isomorphic (provided that $a,b\neq0$). Thus after diagonalizing a form, it suffices to consider the square class of each entry appearing along the diagonal. 
        Example
            alpha = makeGWClass matrix(CC, {{2,3,1},{3,-1,0},{1,0,0}})
            beta = makeGWClass matrix(CC, {{2,4,-1},{4,5,7},{-1,7,9}})
            isIsomorphicForm(alpha,beta)
        Text
            The two forms are isomorphic since they can be diagonalized, after which they can be written as the identity matrix after a change of basis, since every nonzero element is a square over $\mathbb{C}$ (the same is true over any quadratically closed field). Thus we have that the @ITALIC("rank")@ of a form completely determines it over the complex numbers. That is, it provides an isomorphism $\text{GW}(\mathbb{C})\to\mathbb{Z}$. 

            Over the real numbers, the story is a bit different. Since there are two square classes of nonzero real numbers, $\mathbb{R}^\times/\left(\mathbb{R}^\times\right)^2 \cong \left\{\pm 1\right\}$, we have a further invariant which classifies symmetric bilinear forms, called the @TO2(getSignature, "signature")@. This is computed as first diagonalizing, then taking the number of positive entries appearing on the diagonal minus the number of negative entries appearing on the diagonal. 
        Example
            gamma = makeGWClass matrix(RR, {{1,0,0},{0,-1,0},{0,0,1}});
            getSignature gamma
        Text
            Rank and signature completely classify symmetric bilinear forms over the real numbers. 
        Example
            delta = makeGWClass matrix(RR, {{0,0,1},{0,1,0},{1,0,0}});
            isIsomorphicForm(gamma, delta)
        Text
            Over the rational numbers, further invariants must be considered. We first check if the rank and signature (when considered as a real form) all agree. We must then check that their discriminants agree, which is the determinant of any Gram matrix representing the form. This is well defined once we consider its target as landing in square classes of the field, in this case $\mathbb{Q}^{\times}/(\mathbb{Q}^{\times})^{2}$. 
            
            If the abovementioned invariants agree, we must further check whether the @ITALIC("Hasse-Witt invariants")@ agree at all primes. This is an instance of the @ITALIC("Hasse-Minkowski principle")@ which states that quadratic forms are isomorphic over a global field if and only if they are isomorphic over all of its completions (see [S73, IV Theorem 7] or [L05, VI.3.3]). 

            The @ITALIC("Hasse-Witt invariant")@ of a diagonal form $\langle a_1,\ldots,a_n\rangle$ over a field $K$ is defined to be the product $\prod_{i<j} \left( \phi(a_i,a_j) \right)$ where $\phi \colon K \times K \to \left\{\pm 1\right\}$ is any @ITALIC("symbol")@ (see e.g. [MH73, III.5.4] for a definition). It is a classical result of Hilbert that over a local field of characteristic not equal to two, there is a unique symbol, $(-,-)_{p}$ called the @TO2(getHilbertSymbol, "Hilbert symbol")@ ([S73, Chapter III]) computed as follows:
            $$(a,b)_p = \begin{cases} 1 & z^2 = ax^2 + by^2 \text{ has a nonzero solution in } K^3 \\ -1 & \text{otherwise.} \end{cases}$$

            Consider the following example, where we observe that $z^2 = 2x^2 + y^2$ does admit nonzero solutions mod 7, in particular $(x,y,z)=(1,0,3)$. 
        Example
            getHilbertSymbol(2,1,7)
        Text
            The Hasse-Witt invariant will be 1 for almost all primes. In particular, after diagonalizing a form $\beta \cong \left\langle a_1,\ldots,a_n\right\rangle$ then the Hasse invariant at a prime $p$ will automatically be 1 if $p\nmid a_{i}$ for all $i$. Thus we only have finitely many Hasse invariants to compare for any pair of symmetric bilinear forms. 
        Example
            alphaQ = makeGWClass matrix(QQ, {{1,4,7},{4,3,2},{7,2,-1}})
            betaQ = makeGWClass matrix(QQ, {{0,0,1},{0,2,7},{1,7,3}})
            isIsomorphicForm(alphaQ,betaQ)
        Text
            Over finite fields, the signature no longer makes sense as the field fails to be totally ordered. In this case, the rank and discriminant completely determine the form up to isomorphism.
        Example
            alphaF = makeGWClass matrix(GF(7), {{1,2,2},{2,0,1},{2,1,5}})
            betaF = makeGWClass matrix(GF(7), {{2,5,1},{5,6,1},{1,1,3}})
            gammaF = makeGWClass matrix(GF(7), {{0,2,4},{2,3,3},{4,3,1}})
            det getMatrix alphaF 
            det getMatrix betaF
            det getMatrix gammaF
        Text
            As previously indicated, determining if unstable Grothendieck-Witt classes $\alpha$ and $\beta$ over a field $k$ are isomorphic entails checking whether their underlying $\text{GW}(k)$-factors are isomorphic in the Grothendieck-Witt ring and whether their $k^{\times}$-factors agree. 
        Example
            alpha = makeGWuClass(matrix(QQ, {{2,3,1},{3,-1,0},{1,0,0}}), 1)
            beta = makeGWuClass(matrix(QQ, {{2,3,1},{3,-1,0},{1,0,0}}), 4)
            isIsomorphicForm(alpha,beta)
            alpha = makeGWuClass(matrix(CC, {{2,3,1},{3,-1,0},{1,0,0}}), 1)
            beta = makeGWuClass(matrix(CC, {{2,4,-1},{4,5,7},{-1,7,9}}), 1)
            isIsomorphicForm(alpha,beta)
    Caveat
        Over $\mathbb{C}$ and $\mathbb{R}$, the $k^{\times}$-factors of unstable Grothendieck-Witt classes are considered equal if the absolute value of their difference is less than the @TT("linearTolerance")@, which is a positive number specifying the tolerance to which the $k^{\times}$-factors are considered equal. The default tolerance is $10^{-6}$. Over $\mathbb{Q}$ and finite fields, the $k^{\times}$-factors of unstable Grothendieck-Witt classes must agree exactly.
    References
        [S73] J. P. Serre, @ITALIC("A course in arithmetic")@, Springer-Verlag, 1973. 

        [L05] T. Y. Lam, @ITALIC("Introduction to quadratic forms over fields")@, American Mathematical Society, 2005.

        [MH73] J. Milnor and D. Husemoller, @ITALIC("Symmetric bilinear forms")@, Springer-Verlag, 1973.
    SeeAlso
        getRank
        getSignature
        getIntegralDiscriminant
        getRelevantPrimes
        getHasseWittInvariant
        getSumDecomposition
        getSumDecompositionString
///
