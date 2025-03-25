document{
    Key => {getAnisotropicDimensionQQp, (getAnisotropicDimensionQQp, GrothendieckWittClass, ZZ)},
    Headline => "returns the anisotropic dimension of a rational symmetric bilinear form over the p-adic rational numbers",
    Usage => "getAnisotropicDimensionQQp(beta, p)",
    Inputs => {
	GrothendieckWittClass => "beta" => {"over ", TEX///$\mathbb{Q}$///},
	ZZ => "p" => {"a prime number"},
	},
    Outputs => {
        ZZ => {"the rank of the anisotropic part of ", TEX///$\beta$///, " over ", TEX///$\mathbb{Q}_p$///},
	},
    PARA{"This is an implementation of [KC18, Algorithm 8], which computes the anisotropic dimension of rational forms over the ", TEX///$p$///,"-adic rational numbers. Note that any form of rank ", TEX///$\ge 5$///, " is isotropic, so this method will always return 0, 1, 2, 3, or 4."},
    PARA{EM "Citations:"},
    UL{
	{"[KC18] P. Koprowski, A. Czogala, ", EM "Computing with quadratic forms over number fields,", " Journal of Symbolic Computation, 2018."},
    },
    SeeAlso => {"getAnisotropicDimension"}   
}


document{
    Key => {getAnisotropicDimension, (getAnisotropicDimension, GrothendieckWittClass), (getAnisotropicDimension, Matrix)},
    Headline => "returns the anisotropic dimension of a symmetric bilinear form",
    Usage => "getAnisotropicDimension beta",
    Inputs => {
	GrothendieckWittClass => "beta" => {"over a field ", TEX///$k$///, " where ", TEX///$k$///, " is ", TEX///$\mathbb{Q}$///,", ",TEX///$ \mathbb{R}$///,", ",TEX///$\mathbb{C}$///, ", or a finite field of characteristic not 2"},
	},
    Outputs => {
        ZZ => {"the rank of the anisotropic part of ", TEX///$\beta$///},
	},
    PARA{"By the Witt Decomposition Theorem, any non-degenerate form decomposes uniquely as ", TEX///$\beta \cong n \mathbb{H} \oplus \beta_a$///," where the form ", TEX///$\beta_a$///," is anisotropic. The rank of ", TEX///$\beta_a$///, " is called the ", EM "anisotropic dimension", " of ", TEX///$\beta$///, "."},
    PARA{"The anisotropic dimension of a form defined over the rational numbers is the maximum of the ", TO2(getAnisotropicDimensionQQp,"anistropic dimension at each of the completions"), " of ", TEX///$\mathbb{Q}$///, "."},
    SeeAlso => {"getWittIndex", "getAnisotropicDimensionQQp", "getAnisotropicPart"}   
}


document{
    Key => {getWittIndex, (getWittIndex, GrothendieckWittClass)},
    Headline => "returns the Witt index of a symmetric bilinear form",
    Usage => "getWittIndex beta",
    Inputs => {
	GrothendieckWittClass => "beta" => {"denoted by ", TEX///$\beta\in\text{GW}(k)$///, " where ", TEX///$k$///, " is ", TEX///$\mathbb{Q}$///,", ",TEX///$ \mathbb{R}$///,", ",TEX///$\mathbb{C}$///, ", or a finite field of characteristic not 2"},
	},
    Outputs => {
        ZZ => {"the rank of the totally isotropic part of ", TEX///$\beta$///},
	},
    PARA{"By the Witt Decomposition Theorem, any non-degenerate form decomposes uniquely as ", TEX///$\beta \cong n \mathbb{H} \oplus \beta_a$///," where the form ", TEX///$\beta_a$///," is anisotropic. The integer ", TEX///$n$///, " is called the ", EM "Witt index", " of ", TEX///$\beta$///, ". See for instance [L05, I.4.3]."},
    
        PARA{EM "Citations:"},
    UL{
	{"[L05] T.Y. Lam, ", EM "Introduction to quadratic forms over fields,", " American Mathematical Society, 2005."},
    },
    SeeAlso => {"getAnisotropicDimension"}   
}


