doc ///
    Key
        getHilbertSymbol
        (getHilbertSymbol, ZZ, ZZ, ZZ)
        (getHilbertSymbol, QQ, QQ, ZZ)
        (getHilbertSymbol, ZZ, QQ, ZZ)
        (getHilbertSymbol, QQ, ZZ, ZZ)
    Headline
        computes the Hilbert symbol of two rational numbers at a prime
    Usage
        getHilbertSymbol(a, b, p)
    Inputs
        a: QQ
            a nonzero integer or rational number, considered as an element of $\mathbb{Q}_p$
        b: QQ
            a nonzero integer or rational number, considered as an element of $\mathbb{Q}_p$
        p: ZZ
            a prime number
    Outputs
        :ZZ
            the Hilbert symbol $(a,b)_p$
    Description
        Text
            The Hasse-Witt invariant of a diagonal form $\langle a_1,\ldots,a_n\rangle$ over a field $k$ is defined to be the product $\prod_{i<j}  \left( a_i,a_j \right)_p$ where $(-,-)_p$, which is the Hilbert symbol ([S73, Chapter III]) computed as follows:
            $$(a,b)_p = \begin{cases} 1 & \text{if }z^2 = ax^2 + by^2 \text{ has a nonzero solution in } K^3 \\ -1 & \text{otherwise.} \end{cases}$$
            Consider the following example, where we observe that $z^2 = 2x^2 + y^2$ does admit nonzero solutions mod 7, in particular $(x,y,z) = (1,0,3),$ and then by Hensel's lemma, has a solution over $\mathbb{Q}_7$.
        Example
            getHilbertSymbol(2,1,7)
        Text
            In contrast, since $z^2 = 7x^2 + 3y^2$ does not have a  nonzero solution mod 7, the Hilbert symbol will be -1.
        Example
            getHilbertSymbol(7,3,7)
        Text
            Over $\mathbb{Q}_2$, the equation $z^2 = 2x^2 + 2y^2$ has a non-trivial solution, whereas the equation
            $z^2=2x^2+3y^2$ does not. Hence, their Hilbert symbols are 1 and -1, respectively.
        Example
            getHilbertSymbol(2,2,2)
            getHilbertSymbol(2,3,2)
        Text
            Computing Hasse-Witt invariants is a key step in classifying symmetric bilinear forms over the rational numbers, and in particular certifying their @TO2(isAnisotropic,"(an)isotropy")@.
    References
        [S73] J.P. Serre, "A course in arithmetic,"" Springer-Verlag, 1973.
    SeeAlso
        getHilbertSymbolReal
        getHasseWittInvariant
///

doc ///
    Key
        getHilbertSymbolReal
        (getHilbertSymbolReal, QQ, QQ)
        (getHilbertSymbolReal, ZZ, ZZ)
        (getHilbertSymbolReal, ZZ, QQ)
        (getHilbertSymbolReal, QQ, ZZ)
    Headline
        computes the Hilbert symbol of two rational numbers over the real numbers
    Usage
        getHilbertSymbolReal(a, b)
    Inputs
        a: QQ
            a nonzero integer or rational number, considered as an element of $\mathbb{R}$
        b: QQ
            a nonzero integer or rational number, considered as an element of $\mathbb{R}$
    Outputs
        :ZZ
            the Hilbert symbol $(a,b)_{\mathbb{R}}$
    Description
        Text
            The Hasse-Witt invariant of a diagonal form $\langle a_1,\ldots,a_n\rangle$ over a field $k$ is defined to be the product $\prod_{i<j}  \left( a_i,a_j \right)_{\mathbb{R}}$ where $(-,-)_{\mathbb{R}}$, which is the Hilbert symbol ([S73, Chapter III]) computed as follows:
            $$(a,b)_{\mathbb{R}} = \begin{cases} 1 & \text{if }z^2 = ax^2 + by^2 \text{ has a nonzero solution in } \mathbb{R}^3 \\ -1 & \text{otherwise.} \end{cases}$$
            $(a,b)_{\mathbb{R}}$ will equal 1 unless both $a,\,b$ are negative.

            Consider the example, that $z^2=-3x^2-2y^2/3$ does not admit a non-zero solution. Thus:
        Example
            getHilbertSymbolReal(-3, -2/3) == -1
        Text
            Computing Hasse-Witt invariants is a key step in classifying symmetric bilinear forms over the rational numbers, and in particular certifying their @TO2(isAnisotropic,"(an)isotropy")@.
    References
        [S73] J.P. Serre, "A course in arithmetic,"" Springer-Verlag, 1973.
    SeeAlso
        getHilbertSymbol
        getSignature
///