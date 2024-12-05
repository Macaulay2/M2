document{
    Key => {getHilbertSymbol, (getHilbertSymbol, ZZ, ZZ, ZZ), (getHilbertSymbol, QQ, QQ, ZZ), (getHilbertSymbol, ZZ, QQ, ZZ), (getHilbertSymbol, QQ, ZZ, ZZ)},
    Headline => "computes the Hilbert symbol of two rational numbers at a prime",
    Usage => "getHilbertSymbol(a, b, p)",
    Inputs => {
	QQ => "a" => {"a nonzero integer or rational number, considered as an element of ", TEX///$\mathbb{Q}_p$///},
	QQ => "b" => {"a nonzero integer or rational number, considered as an element of ", TEX///$\mathbb{Q}_p$///},
	ZZ => "p" => {"a prime number"},
	},
    Outputs => {
	ZZ => {"the ", EM "Hilbert symbol ", TEX///$(a,b)_p$///},
	},
    PARA{"The ", EM "Hasse-Witt invariant", " of a diagonal form ", TEX///$\langle a_1,\ldots,a_n\rangle$///, " over a field ", TEX///$k$///, " is defined to be the product ", 
	TEX///$\prod_{i<j}  \left( a_i,a_j \right)_p$///, " where ",
    -- " where ", TEX///$\phi \colon k \times k \to \left\{\pm 1\right\}$///, " is any ", EM "symbol", " (see e.g. [MH73, III.5.4] for a definition). It is a classical result of Hilbert that over a local field of characteristic not equal to two, there is a unique symbol, ", 
	TEX///$(-,-)_p$///,  " is the ", EM "Hilbert symbol", " ([S73, Chapter III]) computed as follows:"},
    PARA{TEX///$(a,b)_p = \begin{cases} 1 & \text{if }z^2 = ax^2 + by^2 \text{ has a nonzero solution in } K^3 \\ -1 & \text{otherwise.} \end{cases}$///},
    PARA{"Consider the following example, where we observe that ", TEX///$z^2 = 2x^2 + y^2$///," does admit nonzero solutions mod 7, in particular ", TEX///$(x,y,z) = (1,0,3),$///, 
	" and then by Hensel's lemma, has a solution over ",TEX///$\mathbb{Q}_7$///, "."},
    EXAMPLE lines///
    getHilbertSymbol(2,1,7)
    ///,
    PARA{"In contrast, since ", TEX///$z^2 = 7x^2 + 3y^2$///, 
	" does not have a  nonzero solution mod 7, the Hilbert symbol will be ",
	TEX///$-1.$///},
    EXAMPLE lines///                                                                         
    getHilbertSymbol(7,3,7)                                                               
    ///,                  
    PARA{"Over ",TEX///$\mathbb{Q}_2$///, " the equation ", TEX///$z^2 = 2x^2 + 2y^2$///,
	" has a non-trivial solution, whereas the equation ", TEX///$z^2=2x^2+3y^2$///, 
	" does not. Hence, their Hilbert symbols are 1 and -1, respectively."},
    EXAMPLE lines///                                                                         
    getHilbertSymbol(2,2,2)
    getHilbertSymbol(2,3,2)                                                               
    ///,                  
    PARA{"Computing Hasse-Witt invariants is a key step in classifying symmetric bilinear forms over the rational numbers, and in particular certifying their ", TO2(isIsotropic, "(an)isotropy"), "."},
    PARA{EM "Citations:"},
    UL{
	{"[S73] J.P. Serre, ", EM "A course in arithmetic,", " Springer-Verlag, 1973."},
	{"[MH73] Milnor and Husemoller, ", EM "Symmetric bilinear forms,", " Springer-Verlag, 1973."},
    },
    SeeAlso => {"getHilbertSymbolReal", "getHasseWittInvariant"}
}

document{
    Key => {getHilbertSymbolReal, (getHilbertSymbolReal, QQ, QQ), (getHilbertSymbolReal, ZZ, ZZ), (getHilbertSymbolReal, ZZ, QQ), (getHilbertSymbolReal, QQ, ZZ)},
    Headline => "computes the Hilbert symbol of two rational numbers over the real numbers",
    Usage => "getHilbertSymbolReal(a, b)",
    Inputs => {
	QQ => "a" => {"a nonzero integer or rational number, considered as an element of ", TEX///$\mathbb{R}$///},
	QQ => "b" => {"a nonzero integer or rational number, considered as an element of ", TEX///$\mathbb{R}$///},
	},
    Outputs => {
	ZZ => {"the ", EM "Hilbert symbol ", TEX///$(a,b)_{\mathbb{R}}$///},
	},
    PARA{"The ", EM "Hasse-Witt invariant", " of a diagonal form ", 
	TEX///$\langle a_1,\ldots,a_n\rangle$///, " over a field ", TEX///$k$///, " is defined to be the product ",
	TEX///$\prod_{i<j}  \left( a_i,a_j \right)_{\mathbb{R}}$///, " where ", 
	TEX///$(-,-)_{\mathbb{R}}$///,  " is the ", EM "Hilbert symbol", " ([S73, Chapter III]) computed as follows:"},
    PARA{TEX///$(a,b)_{\mathbb{R}} = \begin{cases} 1 & z^2 = ax^2 + by^2 \text{ has a nonzero solution in } {\mathbb{R}}^3 \\ -1 & \text{otherwise.} \end{cases}$///},
    PARA{TEX///$(a,b)_{\mathbb{R}}$///," will equal 1 unless both ",TEX///$a,\,b$///," are negative."},
    PARA{"Consider the example, that ",TEX///$z^2=-3x^2-2y^2/3$///," does not admit a non-zero solution. Thus:"}, 
     EXAMPLE lines///
    getHilbertSymbolReal(-3, -2/3) == -1
    ///,
    PARA{"Computing Hasse-Witt invariants is a key step in classifying symmetric bilinear forms over the rational numbers, and in particular certifying their ", TO2(isIsotropic, "(an)isotropy"), "."},
    PARA{EM "Citations:"},
    UL{
	{"[S73] J.P. Serre, ", EM "A course in arithmetic,", " Springer-Verlag, 1973."},
	{"[MH73] Milnor and Husemoller, ", EM "Symmetric bilinear forms,", " Springer-Verlag, 1973."},
    },
    SeeAlso => {"getHilbertSymbol", "getSignature"}
}
