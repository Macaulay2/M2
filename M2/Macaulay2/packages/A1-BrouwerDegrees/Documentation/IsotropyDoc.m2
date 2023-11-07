document{
    Key => {isIsotropic, (isIsotropic, GrothendieckWittClass), (isIsotropic, Matrix)},
    Headline => "determines whether a Grothendieck-Witt class is isotropic",
    Usage => "isIsotropic(beta)",
    Inputs => {
	GrothendieckWittClass => "beta" => {"denoted by ", TEX///$\beta\in\text{GW}(k)$///, ", where ", TEX///$k$///, " is the rationals, reals, complex numbers, or a finite field."},
	},
    Outputs => {
        Boolean => {"whether ", TEX///$\beta$///, " is isotropic"},
	},
    PARA{"This is the negation of the boolean-valued ", TO2(isAnisotropic,"isAnisotropic"), ". See documentation there."},    
    SeeAlso => {"isAnisotropic", "HilbertSymbol", "signature"}
}


document{
    Key => {isAnisotropic, (isAnisotropic, GrothendieckWittClass), (isAnisotropic, Matrix)},
    Headline => "determines whether a Grothendieck-Witt class is anisotropic",
    Usage => "isAnisotropic(beta)",
    Inputs => {
	GrothendieckWittClass => "beta" => {"denoted by ", TEX///$\beta\in\text{GW}(k)$///, ", where ", TEX///$k$///, " is the rationals, reals, complex numbers, or a finite field"},
	},
    Outputs => {
        Boolean => {"whether ", TEX///$\beta$///, " is anisotropic"},
	},
    PARA{"Recall a symmetric bilinear form ", TEX///$\beta$///, " is said to be ", EM "isotropic", " if there exists a nonzero vector ", TEX///$v$///, " for which ", TEX///$\beta(v,v) = 0$///, ". Witt's decomposition theorem implies that a non-degenerate symmetric bilinear form decomposes uniquely into an isotropic and an anisotropic part. Certifying (an)isotropy is then an important computational problem when working with the Grothendieck-Witt ring."},
    PARA{"Over ", TEX///$\mathbb{C}$///, ", any form of rank two or higher contains a copy of the hyperbolic form, and hence is isotropic. Thus we can determine anisotropy simply by a consideration of rank."},
    EXAMPLE lines///
    isAnisotropic(gwClass(matrix(CC,{{3}})))
    isAnisotropic(gwClass(matrix(CC,{{2,0},{0,5}})))
    ///,
    PARA{"Forms over ", TEX///$\mathbb{R}$///, " are anisotropic if and only if all its diagonal entries are positive or are negative."},
    EXAMPLE lines///
    isAnisotropic(gwClass(matrix(RR,{{3,0,0},{0,5,0},{0,0,7}})))
    isAnisotropic(gwClass(matrix(RR,{{0,2},{2,0}})))
    ///,
    PARA{"Over finite fields, a form is anisotropic so long as it is nondegenerate, of rank ", TEX///$\le 2$///," and not isomorphic to the hyperbolic form."},
    EXAMPLE lines///
    isAnisotropic(gwClass(matrix(GF(7),{{1,0,0},{0,1,0},{0,0,1}})))
    isAnisotropic(gwClass(matrix(GF(7),{{3,0},{0,3}})))
    ///,
    PARA{"Over ", TEX///$\mathbb{Q}$///, " things become a bit more complicated. We can exploit the local-to-global principle for isotropy (the ", EM "Hasse-Minkowski principle", "), which states that a form is isotropic over ", TEX///$\mathbb{Q}$///, " if and only if it is isotropic over all its completions, meaning all the ", TEX///$p$///, "-adic numbers and ", TEX///$\mathbb{R}$///, " [L05, VI.3.1]. We note, however, the classical result that all forms of rank ", TEX///$\ge 5$///, " in ", TEX///$\mathbb{Q}_p$///, " are isotropic [S73, IV Theorem 6]. Thus isotropy in this range of ranks is equivalent to checking it over the real numbers."},
    EXAMPLE lines///
    beta = gwClass(matrix(QQ,{{1, 0, 2, 0, 3}, {0, 6, 1, 1, -1},{2, 1, 5, 2, 0}, {0, 1, 2, 4, -1}, {3, -1, 0,-1, 1}}));
    isAnisotropic(beta)
    diagonalClass(beta)
    ///,
    PARA{"For forms of rank ", TEX///$\le 4$///, " the problem reduces to computing the maximum anisotropic dimension of the form over local fields. Ternary forms are isotropic away from primes dividing the coefficients of the form in a diagonal basis by e.g. [L05, VI.2.5(2)], so there are only finitely many places to check. Over these ",TO2(relevantPrimes,"relevant primes"), ", isotropy of a form ", TEX///$\beta \in \text{GW}(\mathbb{Q})$///, " over ", TEX///$\mathbb{Q}_p$///," is equivalent to the statement that ", TEX///$(-1,-\text{disc}(\beta))_p = H(\beta)$///, " where ", TEX///$H(\beta)$///, " denotes the ", TO2(HasseWittInvariant,"Hasse-Witt invariant"), " attached to ", TEX///$\beta$///, " and ", TEX///$(-,-)_p$///," is the ", TO2(HilbertSymbol, "Hilbert Symbol"), "."},
    PARA{"A binary form ", TEX///$q$///, " is isotropic if and only if it is isomorphic to the hyperbolic form, which implies in particular that the rank, ", TO2(signature,"signature"), ", and ", TO2(integralDiscriminant,"discriminant"), " of ", TEX///$q$///, " agree with that of ", TEX///$\mathbb{H}=\langle 1,-1\rangle$///, ". " },
    PARA{EM "Citations:"},
    UL{
	
	{"[S73] J.P. Serre, ", EM "A course in arithmetic,", " Springer-Verlag, 1973."},
	{"[L05] T.Y. Lam, ", EM "Introduction to quadratic forms over fields,", " American Mathematical Society, 2005."},
    },
    SeeAlso => {"isIsotropic"}   
}
