doc ///
    Key
        getGlobalA1Degree
        (getGlobalA1Degree, List)
    Headline
        computes the global $\mathbb{A}^{1}$-Brouwer degree of a list of $n$ polynomials in $n$ variables over a field $k$
    Usage
        getGlobalA1Degree L
    Inputs
        L : List
            of polynomials $f=(f_{1}, \ldots, f_{n})$ in the polynomial ring $k[x_{1},\dots,x_{n}]$ where $k$ is $\mathbb{C}$, $\mathbb{Q}$, or a finite field of characteristic not 2. Over $\mathbb{R}$, the user is prompted to instead do the computation over $\mathbb{Q}$ and then base change to $\mathbb{R}$.
    Outputs
        : GrothendieckWittClass
            the class $\text{deg}^{\mathbb{A}^{1}}(f)$ in the Grothendieck-Witt ring $\text{GW}(k)$
    Description
        Text
            Given an endomorphism of affine space $f=(f_{1},\ldots,f_{n}):\mathbb{A}^{n}_{k}\to\mathbb{A}^{n}_{k}$ with isolated zeroes, we may compute its $\mathbb{A}^{1}$-@ITALIC("Brouwer degree")@ valued in the Grothendieck-Witt ring $\text{GW}(k)$. 

            The $\mathbb{A}^{1}$-@ITALIC("Brouwer degree")@ first defined by Morel [M12] is an algebro-geometric enrichment of the classical topological Brouwer degree. Using the tools of motivic homotopy theory, one may associate to an endomorphism of affine space the isomorphism class of a symmetric bilinear form whose invariants encode geometric data about how the morphism transforms space.

            Such an association appears in the work of Eisenbud-Levine [EL77] and Khimshiashvili [K77], wherein the authors develop a symmetric bilinear form whose signature computes the local degree of a smooth map of real manifolds in the case where the Jacobian may vanish on an affine chart. This was proven to agree with Morel's $\mathbb{A}^{1}$-Brouwer degree in work of Kass and Wickelgren [KW19]. A similar production of a symmetric bilinear form is given by work of Scheja and Storch [SS76], which develops a symmetric bilinear form attached to a complete intersection. This was also shown to align with the $\mathbb{A}^{1}$-Brouwer degree in [BW23]. 

            Following recent work [BMP23], the $\mathbb{A}^{1}$-Brouwer degree can be computed as a multivariate @ITALIC("Bézoutian bilinear form")@. The algorithms for producing such a form are developed here. 
        Example
            QQ[x];
            f = {x^2 + 1};
            getGlobalA1Degree f
        Text
            The previous example produces a rank two form with signature zero. This corresponds to the fact that the degree of the complex map $\mathbb{C}\to\mathbb{C}, z\mapsto z^{2}$ has degree two, while the associated real map $\mathbb{R}\to\mathbb{R}, x\mapsto x^{2}$ has global degree zero. 

            Following [M21] we may think of the $\mathbb{A}^{1}$-Brouwer degree $\text{deg}^{\mathbb{A}^1}(f)$ as a quadratically enriched intersection multiplicity of the hyperplanes $V(f_1)\cap \cdots \cap V(f_n)$. As a toy example, consider the curve $y=x(x-1)(x+1)$ intersecting the $x$-axis. 
        Example
            QQ[x,y];
            f = {x^3 - x^2 - y, y};
            getGlobalA1Degree f
        Text
            The rank of this form is three, as cubics over the complex numbers have three roots counted with multiplicity. This form has signature one, which indicates that when the cubic intersects the $x$-axis, when the three points of intersection are counted with a sign corresponding to a right hand rule, the sum equals one.

            The global $\mathbb{A}^{1}$-Brouwer degree can be computed as a sum over the @TO2(getLocalA1Degree, "local degrees")@ at the points in the zero locus of the morphism. In the previous example, we see that $V(f)$ consists of three points on the affine line. We can compute local degrees at all of these and verify that the local degrees sum to the global degree.
        Example
            point1 = ideal(x - 1, y);
            point2 = ideal(x, y);
            getGlobalA1Degree f
            getLocalA1Degree(f, point1)
            getLocalA1Degree(f, point2)
            isIsomorphicForm(getGlobalA1Degree f, addGW(getLocalA1Degree(f, point1), getLocalA1Degree(f, point2))) 
    References
        [BW23] T. Bachmann, K. Wickelgren, "Euler classes: six-functors formalism, dualities, integrality and linear subspaces of complete intersections," @ITALIC("J. Inst. Math. Jussieu")@, 2023. 

        [BMP23] T. Brazelton, S. McKean, S. Pauli, "Bézoutians and the $\mathbb{A}^{1}$-Degree," @ITALIC("Algebra & Number Theory")@, 2023. 

        [EL77] D. Eisenbud, H. Levine, "An algebraic formula for the degree of a $C^{\infty}$ map germ," @ITALIC("Annals of Mathematics")@, 1977. 

        [KW19] J. Kass, K. Wickelgren, "The class of Eisenbud-Khimshashvili-Levine is the local $\mathbb{A}^{1}$-Brouwer degree," @ITALIC("Duke Math J.")@, 2019. 

        [K77] G. Khimshiashvili, "The local degree of a smooth mapping," @ITALIC("Sakharth. SSR Mcn. Akad. Moambe")@, 1977. 

        [M21] S. McKean, "An arithmetic enrichment of Bézout's Theorem," @ITALIC("Math. Ann.")@, 2021. 

        [M12] F. Morel, "$\mathbb{A}^{1}$-Algebraic topology over a field," @ITALIC("Springer Lecture Notes in Mathematics")@, 2012. 

        [SS76] S. Scheja, S. Storch, "Uber Spurfunktionen bei vollstandigen Durchschnitten," @ITALIC("J. Reine Angew. Math.")@, 1975. 
    SeeAlso
        getGlobalUnstableA1Degree
        getLocalA1Degree
        getLocalUnstableA1Degree
        getSumDecomposition
        getSumDecompositionString
///


doc ///
    Key
        getLocalA1Degree
        (getLocalA1Degree, List, Ideal)
    Headline
        computes a local $\mathbb{A}^{1}$-Brouwer degree of a list of $n$ polynomials in $n$ variables over a field $k$ at a prime ideal in the zero locus
    Usage
        getLocallA1Degree(L, p)
    Inputs
        L: List
            of polynomials $f=(f_{1}, \ldots, f_{n})$ in the polynomial ring $k[x_{1},\dots,x_{n}]$ where $k$ is $\mathbb{C}$, $\mathbb{Q}$, or a finite field of characteristic not 2. Over $\mathbb{R}$, the user is prompted to instead do the computation over $\mathbb{Q}$ and then base change to $\mathbb{R}$. 
        p: Ideal
            a prime ideal $p\subset k[x_{1},\dots,x_{n}]$ corresponding to a point in the zero locus $V(f)$
    Outputs
        : GrothendieckWittClass
            the class $\text{deg}_{p}^{\mathbb{A}^{1}}(f)$ in the Grothendieck-Witt ring $\text{GW}(k)$
    Description
        Text
            Given an endomorphism of affine space $f=(f_{1},\ldots,f_{n}):\mathbb{A}^{n}_{k}\to\mathbb{A}^{n}_{k}$ and an isolated zero $p\in V(f)$, we may compute its @ITALIC("local")@ $\mathbb{A}^{1}$-@ITALIC("Brouwer degree")@ valued in the Grothendieck-Witt ring $\text{GW}(k)$. 

            For historical and mathematical background, see @TO2(getGlobalA1Degree, "global A1-degrees")@.
        Example
            T1 = QQ[z_1..z_2];
            f1 = {(z_1 - 1)*z_1*z_2, (3/5)*z_1^2 - (17/3)*z_2^2};
            q = ideal(z_1, z_2);
            r = ideal(z_1-1, z_2^2 - 9/85);
            f1LDq = getLocalA1Degree(f1, q)
            f1LDr = getLocalA1Degree(f1, r)
        Text
            The sum of the local A1-degrees is equal to the global A1-degree:
        Example
            f1LDsum = addGW(f1LDq,f1LDr);
            f1GD = getGlobalA1Degree f1;
            isIsomorphicForm(f1GD,f1LDsum)
    SeeAlso
        getLocalUnstableA1Degree
        getGlobalA1Degree
        getGlobalUnstableA1Degree
        getSumDecomposition
        getSumDecompositionString
///





