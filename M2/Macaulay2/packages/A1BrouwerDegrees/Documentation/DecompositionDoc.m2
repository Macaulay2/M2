doc ///
    Key
        getSumDecomposition
        (getSumDecomposition, GrothendieckWittClass)
        (getSumDecomposition, UnstableGrothendieckWittClass)
    Headline
        produces a simplified diagonal representative of a Grothendieck-Witt class or unstable Grothendieck-Witt class
    Usage
        getSumDecomposition(beta)
    Inputs
        beta: GrothendieckWittClass
            over $\mathbb{C},\mathbb{Q},\mathbb{R}$, or a finite field of characteristic not two
        beta: UnstableGrothendieckWittClass
            over $\mathbb{C},\mathbb{Q},\mathbb{R}$, or a finite field of characteristic not two
    Outputs
        : GrothendieckWittClass
            a Grothendieck-Witt class which is a simplified diagonal representative of the input class
        : UnstableGrothendieckWittClass
            an unstable Grothendieck-Witt class with a simplified diagonal representative of the Grothendieck-Witt class factor
    Description
        Text
            Given a Grothendieck-Witt class or unstable Grothendieck-Witt class over $\mathbb{C},\mathbb{Q},\mathbb{R}$, or a finite field of characteristic not two, this method produces a simplified diagonal representative of the class. The result is a Grothendieck-Witt class or unstable Grothendieck-Witt class decomposed as a sum of some number of hyperbolic and rank one forms. In the unstable case, the result is the unstable Grothendieck-Witt class obtained by applying the method to the Grothendieck-Witt class factor of the unstable Grothendieck-Witt class. 

            We now describe the procedure for decomposing a Grothendieck-Witt class. 
            
            Over $\mathbb{C}$, symmetric bilinear forms are uniquely determined by their rank. Thus the decomposition of a form of rank $n$ is a sum of $\lfloor\frac{n}{2}\rfloor$ hyperbolic forms and a rank one form if $n$ is odd, or $\frac{n}{2}$ hyperbolic forms if $n$ is even.
        Example
            M = matrix(CC, {{1,2,3},{2,4,5},{3,5,6}});
            alpha = makeGWClass M;
            getSumDecomposition alpha
        Text
            Over $\mathbb{R}$, there are two square classes and a form is determined uniquely by its rank and signature [L05, II Proposition 3.5]. The form below is isomorphic to the form $\langle 1,-1,1\rangle$. 
        Example
            N = matrix(RR, {{2.091,2.728,6.747},{2.728,7.329,6.257},{6.747,6.257,0.294}});
            beta = makeGWClass N;
            getSumDecomposition beta
        Text
            Over $\mathbb{Q}$, symmetric bilinear forms decompose into a sum of hyperbolic forms and its @TO2(getAnisotropicPart, "anisotropic part")@. 
        Example
            P = matrix(QQ, {{1,2,3},{2,4,5},{3,5,6}});
            gamma = makeGWClass P;
            getSumDecomposition gamma
        Text
            Over a finite field of characteristic not two, Grothendieck-Witt classes can similarly be diagonalized and decomposed. 
        Example
            Q = matrix(GF(13), {{9,1,7,4},{1,10,3,2},{7,3,6,7},{4,2,7,5}});
            delta = makeGWClass Q;
            getSumDecomposition delta
    References
        [L05] Lam, T. Y., @ITALIC("Introduction to Quadratic Forms over Fields")@, American Mathematical Society, 2005.
    SeeAlso
        getSumDecompositionString
        getAnisotropicPart
        getWittIndex
///

doc ///
    Key
        getSumDecompositionString
        (getSumDecompositionString, GrothendieckWittClass)
        (getSumDecompositionString, UnstableGrothendieckWittClass)
    Headline
        produces a simplified string representation of a Grothendieck-Witt class or unstable Grothendieck-Witt class
    Usage
        getSumDecompositionString(beta)
    Inputs
        beta: GrothendieckWittClass
            over $\mathbb{C},\mathbb{Q},\mathbb{R}$, or a finite field of characteristic not two
        beta: UnstableGrothendieckWittClass
            over $\mathbb{C},\mathbb{Q},\mathbb{R}$, or a finite field of characteristic not two
    Outputs
        : String
            a string representing a simplified diagonal representative of the class
    Description
        Text
            Given a Grothendieck-Witt class or unstable Grothendieck-Witt class over $\mathbb{C},\mathbb{Q},\mathbb{R}$, or a finite field of characteristic not two, this method produces a string representing a simplified diagonal representative of the class. 
            
            See @TO2(getSumDecomposition, "getSumDecomposition")@ for more details on the decomposition procedure.
        Example
            M = matrix(CC, {{1,2,3},{2,4,5},{3,5,6}});
            alpha = makeGWClass M;
            getSumDecompositionString alpha
            N = matrix(RR, {{2.091,2.728,6.747},{2.728,7.329,6.257},{6.747,6.257,0.294}});
            beta = makeGWClass N;
            getSumDecompositionString beta
            P = matrix(QQ, {{1,2,3},{2,4,5},{3,5,6}});
            gamma = makeGWClass P;
            getSumDecompositionString gamma
            Q = matrix(GF(13), {{9,1,7,4},{1,10,3,2},{7,3,6,7},{4,2,7,5}});
            delta = makeGWClass Q;
            getSumDecompositionString delta
    SeeAlso
        getSumDecomposition
        getAnisotropicPart
        getWittIndex
///

doc ///
    Key
        getAnisotropicPart
        (getAnisotropicPart, GrothendieckWittClass)
        (getAnisotropicPart, Matrix)
    Headline
        produces the anisotropic part of a Grothendieck-Witt class
    Usage
        getAnisotropicPart(beta)
    Inputs
        beta: GrothendieckWittClass
            over $\mathbb{C},\mathbb{Q},\mathbb{R}$, or a finite field of characteristic not two
    Outputs
        : GrothendieckWittClass
            the anisotropic part of the Grothendieck-Witt class
    Description
        Text
            By the Witt Decomposition theorem, any Grothendieck-Witt class can be decomposed uniquely into a sum of hyperbolic forms and an anisotropic part $\beta\cong\beta_{a}\oplus n\mathbb{H}$ where the form $\beta_{a}$ is anisotropic. This method returns the anisotropic part $\beta_{a}$ of the Grothendieck-Witt class $\beta$.

            Over the complex and real numbers, this is straightforward, and over finite fields it is a fairly routine computation. Over the rational number, some more sophisticated algorithms are needed from the literature. For this, we implement algorithms developed for number fields by Koprowski and Rothkegel [KR23].
        Example
            alpha = makeDiagonalForm(QQ, (3,-3,2,5,1,-9));
            getAnisotropicPart alpha
    References
        [KR23] P. Koprowski and B. Rothkegel, "The anisotropic part of a quadratic form over a number field," @ITALIC("J. Symbolic Computation")@, 2023. 
    SeeAlso
        getAnisotropicDimension
        getWittIndex
        getSumDecomposition
        getSumDecompositionString
///