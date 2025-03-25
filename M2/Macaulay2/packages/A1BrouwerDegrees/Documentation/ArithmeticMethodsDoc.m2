document{
    Key => {getPadicValuation, (getPadicValuation, ZZ, ZZ), (getPadicValuation, QQ, ZZ)},
    Headline => "p-adic valuation of a rational number",
    Usage => "getPadicValuation(a, p)",
    Inputs => {
	QQ => "a" => {"a non-zero rational number in ", TEX///$\mathbb{Q}_p$///},
	ZZ => "p" => {"a prime number"},
	},
    Outputs =>{
	ZZ => {TEX///$n$///, " where ",TEX///$a=p^n u$///, " and ", TEX///$u$///," is a unit in ", TEX///$\mathbb{Z}_p$///},
        },
    EXAMPLE lines///
    a = 363/7;
    getPadicValuation(a, 11)
    ///,
    }


document {
    Key => {getLocalAlgebraBasis, (getLocalAlgebraBasis, List, Ideal)},
	Headline => "produces a basis for a local finitely generated algebra over a field k",
	Usage => "getLocalAlgebraBasis(L, p)",
	Inputs => {
	    List => "L" => {"of polynomials ", TEX///$f=(f_1, \dots ,f_n)$///, " over the same ring"},
	    Ideal => "p" => {"a prime ideal of an isolated zero"}
	    },
	Outputs => {
	    List => {"of basis elements of the local ",TEX///$k$///,"-algebra ", TEX///$Q_p(f)$/// }
	    },
	PARA {"Given an endomorphism of affine space, ", TEX///$f=(f_1,\dots ,f_n)$///,
			", given as a list of polynomials called ", TT "L", " and the prime ideal of an isolated zero, this command returns a list of basis elements of the local k-algebra ", TEX///$Q_p(f)$///, " by computing a normal basis for ", TEX///$(I:(I:p^{\infty}))$///, " (see [S02, Proposition 2.5])."},
	EXAMPLE lines ///
		 QQ[x,y];
		 f = {x^2 + 1 - y, y};
		 p = ideal(x^2 + 1, y);
		 getLocalAlgebraBasis(f, p) 
	 	 ///,
        PARA{EM "Citations:"},
    UL{
	{"[S02] B. Sturmfels, ", EM "Solving Systems of Polynomial Equations,", " American Mathematical Society, 2002."},
	},
    SeeAlso => {"getLocalA1Degree"}
}
