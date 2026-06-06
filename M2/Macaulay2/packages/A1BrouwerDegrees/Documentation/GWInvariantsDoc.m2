doc ///
    Key
        getSignature
        (getSignature, GrothendieckWittClass)
    Headline
        computes the signature of a symmetric bilinear form over the real numbers or rational numbers
    Usage
        getSignature(beta)
    Inputs
        beta: GrothendieckWittClass
            a symmetric bilinear form defined over $\mathbb{Q}$ or $\mathbb{R}$
    Outputs
        :ZZ
            the signature of the symmetric bilinear form $\beta$
    Description
        Text
            Given a symmetric bilinear form, after diagonalizing it, we can consider the number of positive entries minus the number of negative entries appearing along the diagonal. This is the @ITALIC("signature")@ of a symmetric bilinear form, and is one of the primary invariants used to classify forms. For more information, see @TO2(isIsomorphicForm, "isIsomorphicForm")@. 
        Example
            M = matrix(RR, {{0,0,1},{0,1,0},{1,0,0}});
            beta = makeGWClass M;
            getSignature beta
    SeeAlso
        isIsomorphicForm
        getHilbertSymbol
        getSumDecomposition
        getSumDecompositionString
///

doc ///  
    Key 
        getIntegralDiscriminant
        (getIntegralDiscriminant, GrothendieckWittClass)
    Headline 
        computes the integral discriminant for a rational symmetric bilinear form
    Usage 
        getIntegralDiscriminant(beta)
    Inputs
        beta: GrothendieckWittClass
            denoted by $\beta \in \text{GW}(\mathbb{Q})$
    Outputs
        :ZZ
            an integral square class representative of $\text{disc}(\beta)$
    Description
        Text
            The integral discriminant of a symmetric bilinear form $\beta$ is defined to be the square class of the determinant of the matrix representing $\beta$. This is an invariant of the form, and is used in the classification of forms. 
        Example 
            beta = makeGWClass matrix(QQ, {{1,4,7},{4,3,-1},{7,-1,5}});
            getIntegralDiscriminant beta
            getDiagonalClass beta
    SeeAlso
        isIsomorphicForm
///


doc /// 
    Key
        getHasseWittInvariant
        (getHasseWittInvariant, GrothendieckWittClass, ZZ)
        (getHasseWittInvariant, List, ZZ)
    Headline
        computes the Hasse-Witt invariant at a prime $p$ for the quadratic form of the Grothendieck-Witt class
    Usage
        getHasseWittInvariant(beta, p)
    Inputs
        beta: GrothendieckWittClass
            denoted by $\beta \in \text{GW}(\mathbb{Q})$
        p: ZZ
            a prime number  
    Outputs
        :ZZ
            the Hasse-Witt invariant of $\beta$ at the prime $p$
    Description
        Text
            The Hasse-Witt invariant of a diagonal form $\langle a_1,\ldots,a_n\rangle$ over a field $K$ is defined to be the product $\prod_{i<j} \left(a_i,a_j\right)_p$ where $(-,-)_p$ is the @TO2(getHilbertSymbol, "Hilbert symbol")@. 
            
            The Hasse-Witt invariant of a form will be equal to 1 for almost all primes.  In particular, after diagonalizing a form $\beta \cong \left\langle a_1,\ldots,a_n\right\rangle$, the Hasse-Witt invariant at a prime $p$ will be 1 automatically if $p\nmid a_i$ for all $i$. Thus we only have to compute the Hasse-Witt invariant at @TO2(getRelevantPrimes, "primes dividing diagonal entries")@.
        Example
            beta = makeGWClass matrix(QQ, {{1,4,7},{4,3,-1},{7,-1,5}});
            getHasseWittInvariant(beta, 7)
    SeeAlso
        isIsomorphicForm
        getRelevantPrimes
///       

doc ///
    Key
        getRelevantPrimes
        (getRelevantPrimes, GrothendieckWittClass)
    Headline
        outputs a list containing all primes $p$ where the Hasse-Witt invariant of a symmetric bilinear form is nontrivial
    Usage
        getRelevantPrimes(beta)
    Inputs
        beta: GrothendieckWittClass
            denoted by $\beta \in \text{GW}(\mathbb{Q})$
    Outputs
        : List
            a finite list of primes $(p_1,\ldots,p_r)$ containing all primes $p$ where the Hasse-Witt invariant $\phi_p(\beta)$ is nontrivial
    Description
        Text
            It is a classical result that the Hasse-Witt invariants of a quadratic form are equal to 1 for all but finitely many primes (see e.g. [S73, IV Section 3.3]). As the Hasse-Witt invariants are computed as a product
            of Hilbert symbols of the pairwise entries appearing on a diagonalization of the symbol, it suffices to consider primes dividing diagonal entries.
        Example
            beta = makeDiagonalForm(QQ, (6,7,22));
            getRelevantPrimes(beta)
    References
        [S73] J.P. Serre, "A course in arithmetic," Springer-Verlag, 1973.
    SeeAlso
        getHasseWittInvariant
///

doc ///
    Key
        getRank
        (getRank, GrothendieckWittClass)
        (getRank, Matrix)
    Headline
        calculates the rank of a symmetric bilinear form
    Usage
        getRank(beta)
    Inputs
        beta: GrothendieckWittClass
            a symmetric bilinear form defined over $\mathbb{Q}$
    Outputs
        : ZZ
            the rank of the symmetric bilinear form $\beta$
    Description
        Text
            The rank of a symmetric bilinear form is the dimension of the vector space spanned by the vectors that are orthogonal to all other vectors in the space. This is equivalent to the number of non-zero entries on the diagonal of a diagonalized form.
        Example
            beta = makeDiagonalForm(QQ, (3,5,7,11))
            getRank beta
    SeeAlso
        isIsomorphicForm
        getSignature
///