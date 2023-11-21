document{
    Key => {gwIsomorphic, (gwIsomorphic, GrothendieckWittClass, GrothendieckWittClass)},
    Headline => "determines whether two Grothendieck Witt classes are isomorphic over CC, RR, QQ, or a finite field.",
    Usage => "gwIsomorphic(alpha,beta)",
    Inputs => {
	GrothendieckWittClass => "alpha" => {"denoted by ",TEX///$\alpha$///},
	GrothendieckWittClass => "beta" => {"denoted by ",TEX///$\beta$///},
	},
    Outputs => {
	Boolean => {"returns true or false depending on whether two Grothendieck Witt classes are equal in the Grothendieck-Witt ring"},
	},
    PARA{"Given two matrices representing symmetric bilinear forms over a field ", TEX///$k$///, ", it is a fundamental question to ask when they are representing the same symmetric bilinear form, i.e. when they are equal in the Grothendieck-Witt ring ", TEX///$\text{GW}(k)$///,"."},
    
    PARA{EM "Sylvester's Law of Inertia", " proves that any symmetric bilinear form can be diagonalized into a block sum of rank one symmetric bilinear forms. Since the rank one forms ", TEX///$\langle a \rangle \colon k \times k \to k$///, ", ", TEX///$(x,y) \mapsto axy$///, " and ", TEX///$\langle ab^2 \rangle \colon k \times k \to k$///, ", ", TEX///$(x,y) \mapsto ab^2xy$///, " differ by a change of basis in the ground field, it follows they are isomorphic (provided that ", TEX///$a,b\ne 0$///, "). Thus after diagonalizing a form, it suffices to consider the square class of each entry appearing along the diagonal. Consider the following example over the complex numbers:"},
    EXAMPLE lines ///
    alpha = gwClass(matrix(CC,{{2,3,1},{3,-1,0},{1,0,0}}))
    beta = gwClass(matrix(CC,{{2,4,-1},{4,5,7},{-1,7,9}}))
    gwIsomorphic(alpha,beta)
    ///,
    PARA{"The two forms are isomorphic since they can be diagonalized, after which they can be rewritten as the identity matrix after a change of basis, since every nonzero element is a square class over ", TEX///$\mathbb{C}$///, " (the same is true for any quadratically closed field). Thus we have that the ", EM "rank", " of a form completely determines it over the complex numbers. That is, it provides an isomorphism ", TEX///$\text{GW}(\mathbb{C}) \to \mathbb{Z}$/// ,"."},
    PARA{"Over the reals, the story is a bit different. Since there are two classes over the reals, ", TEX///$ \mathbb{R}^\times / \left(\mathbb{R}^\times\right)^2 \cong \left\{\pm 1\right\}$///," we have a further invariant which classifies symmetric bilinear forms, called the ", TO2(signature, "signature"), ". This is computed as first diagonalizing, then taking the number of positive entries appearing on the diagonal minus the number of negative entries appearing on the diagonal."},
    EXAMPLE lines ///
    gamma = gwClass(matrix(RR,{{1,0,0},{0,-1,0},{0,0,1}}));
    signature(gamma)
    ///,
    PARA{"Rank and signature completely classify symmetric bilinear forms over the reals."},
    EXAMPLE lines ///
    delta = gwClass(matrix(RR,{{0,0,1},{0,1,0},{1,0,0}}));
    gwIsomorphic(gamma,delta)
    ///,
    PARA{"Over finite fields, rank is still an invariant of a form, however signature no longer makes sense as the field is not totally ordered. Instead we consider the ", EM "discriminant", " of the non-degenerate symmetric bilinear form, which is the determinant of any Gram matrix representing the form. The discriminant is well-defined once we consider its target as landing in square classes of the field. ", TEX///$\text{GW}(\mathbb{F}_q) \to \mathbb{F}_q^\times / \left(\mathbb{F}_q^\times\right)^2$///, ". Over finite fields we look to compare both rank and discriminant of a form."},
    EXAMPLE lines///
    alphaF = gwClass(matrix(GF(7),{{1,2,2},{2,0,1},{2,1,5}}))
    betaF = gwClass(matrix(GF(7),{{2,5,1},{5,6,1},{1,1,3}}))
    gammaF = gwClass(matrix(GF(7),{{0,2,4},{2,3,3},{4,3,1}}))
    det(alphaF.matrix)    
    det(betaF.matrix)
    det(gammaF.matrix)
    ///,
    PARA{"We see that ", TEX///$\text{disc}(\alpha)$///, " is a square, while the discriminants of ", TEX///$\beta$///, " and ", TEX///$\gamma$///, " are not. Therefore we see that ", TEX///$\beta \cong \gamma$///, " but neither of them are isomorphic to ", TEX///$\alpha$///, "."},
    EXAMPLE lines///
    gwIsomorphic(alphaF,betaF)
    gwIsomorphic(alphaF,gammaF)
    gwIsomorphic(betaF,gammaF)
    ///,
    PARA{"Over the rationals, further invariants must be considered. We first check if the rank, discriminant, and signature (when considered as a real form) all agree. If so, we must further check whether the ", EM "Hasse-Witt invariants", " agree at all primes. This is an instance of the ", EM "Hasse-Minkowski principle", " which states that quadratic forms are isomorphic over a global field if and they are isomorphic over all its completions (see [S73, IV Theorem 7] or [L05, VI.3.3])."},
    PARA{"The ", EM "Hasse-Witt invariant", " of a diagonal form ", TEX///$\langle a_1,\ldots,a_n\rangle$///, " over a field ", TEX///$K$///, " is defined to be the product ", TEX///$\prod_{i<j} \left( \phi(a_i,a_j) \right)$///, " where ", TEX///$\phi \colon K \times K \to \left\{\pm 1\right\}$///, " is any ", EM "symbol", " (see e.g. [MH73, III.5.4] for a definition). It is a classical result of Hilbert that over a local field of characteristic not equal to two, there is one and only symbol, ", TEX///$(-,-)_p$///,  " called the ", TO2(HilbertSymbol,"Hilbert symbol"), " ([S73, Chapter III]) computed as follows:"},
    PARA{TEX///$(a,b)_p = \begin{cases} 1 & z^2 = ax^2 + by^2 \text{ has a nonzero solution in } K^3 \\ -1 & \text{otherwise.} \end{cases}$///},
    PARA{"Consider the following example, where we observe that ", TEX///$z^2 = 2x^2 + y^2$///," does admit nonzero solutions mod 7, in particular ", TEX///$(x,y,z) = (1,0,3)$///, ":"},
    EXAMPLE lines///
    HilbertSymbol(2,1,7)
    ///,
    PARA{"The Hasse invariant will be 1 for almost all primes. In particular after diagonalizing a form ", TEX///$\beta \cong \left\langle a_1,\ldots,a_n\right\rangle$///, " then the Hasse invariant at a prime ", TEX///$p$///, " will be 1 automatically if ", TEX///$p\nmid a_i$///, " for all ", TEX///$i$///, ". Thus we only have finitely many Hasse invariants to compare for any pair of symmetric bilinear forms."},
    EXAMPLE lines///
    alphaQ = gwClass(matrix(QQ,{{1,4,7},{4,3,2},{7,2,-1}}))
    betaQ = gwClass(matrix(QQ,{{0,0,1},{0,2,7},{1,7,3}}))
    gwIsomorphic(alphaQ,betaQ)
    ///,
    PARA{EM "Citations:"},
    UL{
	
	{"[S73] J.P. Serre, ", EM "A course in arithmetic,", " Springer-Verlag, 1973."},
	{"[L05] T.Y. Lam, ", EM "Introduction to quadratic forms over fields,", " American Mathematical Society, 2005."},
	{"[MH73] Milnor and Husemoller, ", EM "Symmetric bilinear forms,", " Springer-Verlag, 1973."},
    },
    SeeAlso => {"signature", "sumDecomposition", "sumDecompositionString"}
}

