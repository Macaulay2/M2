doc ///
    Key
        getGlobalUnstableA1Degree
        (getGlobalUnstableA1Degree, RingElement)
        (getGlobalUnstableA1Degree, RingElement, RingElement)
        [getGlobalUnstableA1Degree, linearTolerance]
    Headline
        computes the global unstable $\mathbb{A}^{1}$-Brouwer degree of a pointed rational function $f/g:\mathbb{P}^{1}_{k}\to\mathbb{P}^{1}_{k}$
    Usage
        getGlobalUnstableA1Degree(q)
        getGlobalUnstableA1Degree(f, g)
    Inputs
        q: RingElement
            a pointed rational function $f/g$ where $f,g\in k[x]$ are coprime polynomials over a field $k$ of characteristic not 2 and $g$ not identically zero. Over $\mathbb{C}$, the two-input variant is to be used as fraction fields are not supported over $\mathbb{C}$. Over $\mathbb{R}$, the user is prompted to instead do the computation over $\mathbb{Q}$ and then base change to $\mathbb{R}$.
        f: RingElement
            a polynomial $f\in k[x]$ where $k$ is a field of characteristic not 2
        g: RingElement
            a polynomial $g\in k[x]$ where $k$ is a field of characteristic not 2 and $g$ is not identically zero, such that $f/g$ is a pointed rational function
        linearTolerance => RR
            a positive real number used to determine whether roots of @TT("f")@ and @TT("g")@ are considered equal when working over the inexact field $\mathbb{C}$, used to put rational functions over $\mathbb{C}$ in reduced form by cancelling roots that agree up to the tolerance
    Outputs
        : UnstableGrothendieckWittClass
            the class $\text{deg}^{\mathbb{A}^{1}}(f/g)$ in the unstable Grothendieck-Witt group $\text{GW}^{u}(k)$
    Description
        Text
            Given a pointed rational function $f/g:\mathbb{P}^{1}_{k}\to\mathbb{P}^{1}_{k}$ (where $(f/g)(\infty)=\infty$), we may compute its @ITALIC("global unstable")@ $\mathbb{A}^{1}$-@ITALIC("Brouwer degree")@ valued in the unstable Grothendieck-Witt group $\text{GW}^{u}(k):=\text{GW}(k)\times_{k^{\times}/(k^{\times})^{2}}k^{\times}$.

            Morel's $\mathbb{A}^{1}$-Brouwer degree generalizes the classical Brouwer degree by associating to an endomorphism of the sphere a class in the Grothendieck-Witt ring of non-degenerate symmetric bilinear forms. While this morphism is an isomorphism in dimensions two and above, it is only surjective in dimension one [M12]. In this case, a computation of Morel [M12] and Cazanave [C12] furnish an isomorphism $[\mathbb{P}^{1}_{k},\mathbb{P}^{1}_{k}]\cong\text{GW}^{u}(k)$. 

            Building on Cazanave's work, Kass and Wickelgren [KW20] and later Igieobo and coauthors [I+24] prove that there is an explicit bilinear form associated to the rational function $f/g$ in both the local and global cases, and provide a local-to-global formula for the degree dependent on the configuration of the zeroes of the rational function. 

            For additional historical and mathematical background about $\mathbb{A}^{1}$-degrees and its relationship to $\mathbb{A}^{1}$-algebraic topology more generally, see @TO2(getGlobalA1Degree, "global A1-degrees")@.

            In the case of the global unstable $\mathbb{A}^{1}$-Brouwer degree, the class is represented by a variant of the @ITALIC("Bézoutian bilinear form")@ [C12, Theorem 3.6]. 
        Example
            frac QQ[x];
            q = (x^5 - 6*x^4 + 11*x^3 - 2*x^2 - 12*x + 8)/(x^4 - 5*x^2 + 7*x + 1);
            getGlobalUnstableA1Degree q
        Text
            The rank of this form is of rank five, which agrees with the number of zeroes of the rational function counted with multiplicity over the complex numbers. 

            In the unstable setting, however, the global unstable $\mathbb{A}^{1}$-Brouwer degree is not computed as the sum of local $\mathbb{A}^{1}$-Brouwer degrees at the zeroes of the rational function in the unstable Grothendieck-Witt ring $\text{GW}^{u}(k)$. Instead, it is computed as the @TO2(addGWuDivisorial, "divisorial sum")@ which depends on the divisor of points given by the zeroes of the rational function [I+24].
        Example
            deg1 = getLocalUnstableA1Degree(q, -1)
            deg2 = getLocalUnstableA1Degree(q, 1)
            deg3 = getLocalUnstableA1Degree(q, 2)
            degSum = addGWuDivisorial({deg1, deg2, deg3}, {-1, 1, 2})
            isIsomorphicForm(degSum, getGlobalUnstableA1Degree q)
	Text
	    Since $f/g$ is assumed to be reduced, $f$ and $g$ do not share a common factor. If the user chooses functions $f$ and $g$ with a common factor, the reduction is computed, checked for pointedness, and the degree computation run on the reduction. Over $\mathbb{Q}$ and $\mathbb{F}_{q}$, rational functions are automatically reduced, and computing @TT("getGlobalUnstableA1Degree(f/g)")@ agrees with computing @TT("getGlobalUnstableA1Degree(f,g)")@ as the computation is done on the underlying rational function. Over $\mathbb{C}$, we consider roots of $f$ and $g$ to be equal if the absolute value of their difference is less than that of the  @TT("linearTolerance")@. The user may specify a tolerance for this cancellation via the option @TT("linearTolerance")@, with the default being @TT("1e-6")@.
	Example
	    R = CC[x];
	    f = (x-1)*(x-2)*(x-3);
	    g = (x-1)*(x-4);
	    getGlobalUnstableA1Degree(f,g,linearTolerance => 1e-6)
    References
        [C12] C. Cazanave, "Algebraic homotopy classes of rational functions," @ITALIC("Ann. Scient. Ec. Norm. Sup.")@, 2012.

        [I+24] J. Igieobo, et. al., "Motivic configurations on the line," @TT("arXiv: 2411.15347")@, 2024. 

        [KW20] J. Kass, K. Wickelgren, "A Classical Proof that the Algebraic Homotopy Class of a Rational Function is the Residue Pairing," @ITALIC("Linear Algebra Appl.")@, 2020.

        [M12] F. Morel, "$\mathbb{A}^{1}$-Algebraic topology over a field," @ITALIC("Springer Lecture Notes in Mathematics")@, 2012.
    SeeAlso
        getLocalUnstableA1Degree
        getLocalA1Degree
        getGlobalA1Degree
        getSumDecomposition
        getSumDecompositionString
///

doc ///
    Key
        getLocalUnstableA1Degree
        (getLocalUnstableA1Degree, RingElement, RingElement)
        (getLocalUnstableA1Degree, RingElement, Number)
        (getLocalUnstableA1Degree, RingElement, RingElement, Number)
        (getLocalUnstableA1Degree, RingElement, RingElement, RingElement)
        [getLocalUnstableA1Degree, linearTolerance]
    Headline
        computes a local unstable $\mathbb{A}^{1}$-Brouwer degree of a pointed rational function $f/g:\mathbb{P}^{1}_{k}\to\mathbb{P}^{1}_{k}$ at a root $p\in\mathbb{P}^{1}_{k}$
    Usage
        getLocalUnstableA1Degree(q, p)
        getLocalUnstableA1Degree(f, g, p)
    Inputs
        q: RingElement
            a pointed rational function $f/g$ where $f,g\in k[x]$ are coprime polynomials over a field $k$ of characteristic not 2 and $g$ not identically zero. Over $\mathbb{C}$ the two-input variant is to be used as fraction fields are not supported over $\mathbb{C}$. Over $\mathbb{R}$, the user is prompted to instead do the computation over $\mathbb{Q}$ and then base change to $\mathbb{R}$.
        f: RingElement
            a polynomial $f\in k[x]$ where $k$ is a field of characteristic not 2
        g: RingElement
            a polynomial $g\in k[x]$ where $k$ is a field of characteristic not 2 and $g$ is not identically zero, such that $f/g$ is a pointed rational function
        p: RingElement
            a point $p\in\mathbb{P}^{1}_{k}$ corresponding to a root of the rational function $f/g$ in the field $k$
        p: Number
            a point $p\in\mathbb{P}^{1}_{k}$ corresponding to a root of the rational function $f/g$ in the field $k$
        linearTolerance => RR
            a positive real number used to determine whether roots of @TT("f")@ and @TT("g")@ are considered equal when working over the inexact field $\mathbb{C}$, used to put rational functions over $\mathbb{C}$ in reduced form by cancelling roots that agree up to the tolerance
    Outputs
        : UnstableGrothendieckWittClass
            the class $\text{deg}_{p}^{\mathbb{A}^{1}}(f/g)$ in the unstable Grothendieck-Witt group $\text{GW}^{u}(k)$
    Description
        Text
            Given a pointed rational function $f/g:\mathbb{P}^{1}_{k}\to\mathbb{P}^{1}_{k}$ (where $(f/g)(\infty)=\infty$) and a zero $p\in\mathbb{A}^{1}_{k}$ (as $f/g$ is pointed), we may compute its @ITALIC("local unstable")@ $\mathbb{A}^{1}$-@ITALIC("Brouwer degree")@ valued in the unstable Grothendieck-Witt group $\text{GW}^{u}(k)$.

            For mathematical background on the local unstable $\mathbb{A}^{1}$-Brouwer degree, see @TO2(getGlobalUnstableA1Degree, "global unstable A1-degrees")@.

            If the rational function is non-reduced, then the reduction is computed, checked for pointedness, and the local degree computation run on the reduction. See @TO2(getGlobalUnstableA1Degree, "global unstable A1-degrees")@ for more details.
        Example
            frac QQ[x];
            q = (x^2 + x - 2)/(3*x + 5);
            getLocalUnstableA1Degree(q, -2)
    SeeAlso
        getGlobalUnstableA1Degree
        getLocalA1Degree
        getGlobalA1Degree
        getSumDecomposition
        getSumDecompositionString
///
