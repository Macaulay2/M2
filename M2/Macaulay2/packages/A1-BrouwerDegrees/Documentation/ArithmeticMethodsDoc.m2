-- document {
--     Key => {(squarefreePart, QQ), (squarefreePart, ZZ), squarefreePart},
-- 	Headline => "smallest magnitude representative of a square class over the rationals or integers",
-- 	Usage => "squarefreePart(q)",
-- 	Inputs => {
-- 	    QQ => "q" => {"a rational number"},
-- 	    -- ZZ => "n" => {"an integer"}
-- 	    },
-- 	Outputs => {
-- 	    ZZ => { "the smallest magnitude integer in the square class of ", TT "n"}
-- 	    },
-- 	PARA {"Given a rational number (or integer), ", TT "q", ", this command outputs the smallest magnitude integer, ",
--                 TEX///$m$///, ", such that ", TEX///$q=lm$///, " for some rational number (or integer) ",
-- 				TEX///$l$///, "."},
-- 	EXAMPLE lines ///
-- 		 squarefreePart(15/72)
-- 		 squarefreePart(-1/3)
-- 	 	 ///,
--         }
    
    
-- document{
--     Key => {(legendreBoolean, RingElement), legendreBoolean},
--     Headline => "Basic Legendre symbol over a finite field",
--     Usage => "legendreBoolean(a)",
--     Inputs => {
-- 	RingElement => "a" => {"Any element in a finite field ", TEX///$a\in \mathbb{F}_q$///, "."},
-- 	},
--     Outputs =>{
-- 	Boolean => {"Whether ", TEX///$a$///, " is a square in ", TEX///$\mathbb{F}_q$///, "."},
-- 	},
--     PARA{"Given an element of a finite field, will return a Boolean checking if it is a square."},
--     EXAMPLE lines///
--     a = sub(-1,GF(5));
--     legendreBoolean(a)
--     b = sub(-1,GF(7));
--     legendreBoolean(b)
--     ///,
--     }

document{
    Key => {padicValuation, (padicValuation, ZZ, ZZ), (padicValuation, QQ, ZZ)},
    Headline => "p-adic valuation of a rational number or integer",
    Usage => "padicValuation(a, p)",
    Inputs => {
	ZZ => "a" => {"a non-zero rational number in ", TEX///$\mathbb{Q}_p$///},
	ZZ => "p" => {"a rational prime number"},
	},
    Outputs =>{
	ZZ => {"an integer ", TEX///$n$///, " where ",TEX///$a=p^n u$///, " and ", TEX///$u$///," is a unit in ", TEX///$\mathbb{Z}_p$///},
        },
    EXAMPLE lines///
    a = 363/7;
    padicValuation(a, 11)
    ///,
    }


document {
    Key => {localAlgebraBasis, (localAlgebraBasis, List, Ideal)},
	Headline => "produces a basis for a local finitely generated algebra over a field k",
	Usage => "localAlgebraBasis(L,p)",
	Inputs => {
	    List => "L" => {"of polynomials ", TEX///$f=(f_1, \dots ,f_n)$///, " over the same ring"},
	    Ideal => "p" => {"a prime ideal of an isolated zero"}
	    },
	Outputs => {
	    List => {"of basis elements of the local ",TEX///$k$///,"-algebra ", TEX///$Q_p(f)$/// }
	    },
	PARA {"Given an endomorphism of affine space, ", TEX///$f=(f_1,\dots ,f_n)$///,
			", given as a list of polynomials called ", TT "L", " and the prime ideal of an isolated zero, this command returns a list of basis elements of the local k-algebra ", TEX///$Q_p(f)$///, " by computing a normal basis for ", TEX///$(I:(I:p^{\infty}))$///, " (vis. [S02, Proposition 2.5])."},
	EXAMPLE lines ///
		 QQ[x,y];
		 f = {x^2+1-y,y};
		 p = ideal(x^2+1,y);
		 localAlgebraBasis(f,p) 
	 	 ///,
        PARA{EM "Citations:"},
    UL{
	
	{"[S02] B. Sturmfels, ", EM "Solving Systems of Polynomial Equations,", " American Mathematical Society, 2002."},
	},
    SeeAlso => {"localA1Degree"}
}
