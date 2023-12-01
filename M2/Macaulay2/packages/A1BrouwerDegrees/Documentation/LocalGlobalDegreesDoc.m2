document {
    Key => {globalA1Degree, (globalA1Degree, List)},
    Headline => "computes a global A1-Brouwer degree of a list of n polynomials in n variables over a field k",
    Usage => "globalA1Degree(L)",
    Inputs => {
	List => "L" => {"of polynomials ", TEX///$f = (f_1, \ldots, f_n)$///, " in the polynomial ring ", TEX///$k[x_1,\ldots,x_n]$///, " over a field ", TEX///$k$///}
	},
    Outputs => {
	GrothendieckWittClass => {"the class ", TEX///$\text{deg}^{\mathbb{A}^1}(f)$///, " in the Grothendieck-Witt ring ", TEX///$\text{GW}(k)$///}
	},
    PARA{"Given an endomorphism of affine space ", TEX///$f=(f_1,\dots ,f_n) \colon \mathbb{A}^n_k \to \mathbb{A}^n_k$///, " with isolated zeros, we may compute its ", TEX///$\mathbb{A}^1$///, EM "-Brouwer degree", " valued in the Grothendieck-Witt ring ", TEX///$\text{GW}(k)$///, "."
	},
    PARA{"The ",
	TEX///$\mathbb{A}^1$///,
	EM "-Brouwer degree",
	" first defined by Morel [M12] is an algebro-geometric enrichment of the classical topological Brouwer degree. Using the tools of motivic homotopy theory, one may associate to an endomorphism of affine space the isomorphism class of a symmetric bilinear form whose invariants encode geometric data about how the morphism transforms space."
        },
    PARA{"Such an association appears in the work of Eisenbud-Levine [EL77] and Khimshiashvili [K77], wherein the authors develop a symmetric bilinear form whose signature computes the local degree of a smooth map of real manifolds in the case where the Jacobian may vanish on an affine chart. This was proven to agree with Morel's ", TEX///$\mathbb{A}^1$///, "-Brouwer degree in work of Kass and Wickelgren [KW19]. A similar production of a symmetric bilinear form is given by work of Scheja and Storch [SS76], which develops a symmetric bilinear form attached to a complete intersection. This was also shown to align with the ", TEX///$\mathbb{A}^1$///, "-Brouwer degree in [BW23]."},
    PARA{"Following recent work of B. McKean and Pauli [BMP23], the ", TEX///$\mathbb{A}^1$///, "-Brouwer degree can be computed as a multivariate ", EM "Bezoutian bilinear form.", " The algorithms for producing such a form are developed here."},
    EXAMPLE lines ///
    QQ[x];
    f = {x^2+1};
    globalA1Degree(f)
    ///,
    PARA{"The previous example produces a rank two form with signature zero. This corresponds to the fact that the degree of the complex map ", TEX///$\mathbb{C}\to\mathbb{C},\ z\mapsto z^2$///, " has degree two, while the associated real map ", TEX///$\mathbb{R}\to\mathbb{R},\ x\mapsto x^2$///, " has global degree zero."},
    PARA{"Following [M21] we may think about the ",TEX///$\mathbb{A}^1$///, "-Brouwer degree ", TEX///$\text{deg}^{\mathbb{A}^1}(f)$///, " as a quadratically enriched intersection multiplicity of the hyperplanes ", TEX///$V(f_1)\cap \cdots \cap V(f_n).$///, " As a toy example, consider the curve ", TEX///$y=x(x-1)(x+1)$///, " intersecting the ", TEX///$x$///, "-axis."},
    EXAMPLE lines ///
    QQ[x,y];
    f = {x^3 - x^2 - y, y};
    globalA1Degree(f)
    ///,
    PARA{"The rank of this form is three, as cubics over the complex numbers have three roots counted with multiplicity. This form has signature one, which indicates that when the cubic intersects the ", TEX///$x$///, "-axis, when the three points of intersection are counted with a sign corresponding to a right hand rule, the sum equals one."},
    
    PARA{"The global ", TEX///$\mathbb{A}^1$///, "-Brouwer degree can be computed as a sum over the ", TO2(localA1Degree,"local degrees"), " at the points in the zero locus of the morphism. In the previous example, we see that ", TEX///$V(f)$///, " consists of three points on the affine line. We can compute local degrees at all of these and verify that the local degrees sum to the global degree:"},
    EXAMPLE lines///
    QQ[x,y];
    f = {x^3 - x^2 - y, y};
    point1 = ideal{x-1, y};
    point2 = ideal{x, y};
    globalA1Degree(f);
    localA1Degree(f,point1);
    localA1Degree(f,point2);
    gwIsomorphic(globalA1Degree(f), gwAdd(localA1Degree(f,point1), localA1Degree(f,point2)));
    ///,
    
    PARA{EM "Citations:"},
    UL{
	
	{"[BW23] T. Bachmann, K. Wickelgren, ", EM "Euler classes: six-functors formalism, dualities, integrality and linear subspaces of complete intersections,", " J. Inst. Math. Jussieu, 2023."},
	{"[EL77] D. Eisenbud, H. Levine, ", EM "An algebraic formula for the degree of a C^infinity map germ,", " Annals of Mathematics, 1977."},
	{"[K77] G. Khimshiashvili, ", EM "The local degree of a smooth mapping,", " Sakharth. SSR Mcn. Akad. Moambe, 1977."},
	{"[KW19] J. Kass, K. Wickelgren, ", EM "The class of Eisenbud-Khimshashvili-Levine is the local A1-Brouwer degree,", " Duke Math J., 2019."},
	{"[M21] S. McKean, ", EM "An arithmetic enrichment of Bezout's Theorem,", " Math. Ann., 2021."},
	{"[M12] F. Morel, ", EM "A1-Algebraic topology over a field,", " Springer Lecture Notes in Mathematics, 2012."},
	{"[BMP23] T. Brazelton, S. McKean, S. Pauli, ", EM "Bezoutians and the A1-Degree,", " Algebra & Number Theory, 2023."},
	{"[SS76] S. Scheja, S. Storch, ", EM "Uber Spurfunktionen bei vollstandigen Durchschnitten,", " J. Reine Angew. Math., 1975."},
	},
    SeeAlso => {"localA1Degree", "sumDecomposition", "sumDecompositionString"}
    }

document {
    Key => {localA1Degree, (localA1Degree, List, Ideal)},
    Headline => "computes a local A1-Brouwer degree of a list of n polynomials in n variables over a field k at a prime ideal in the zero locus",
    Usage => "locallA1Degree(L,p)",
    Inputs => {
	List => "L" => {"of polynomials ", TEX///$f = (f_1, \ldots, f_n)$///, " in the polynomial ring ", TEX///$k[x_1,\ldots,x_n]$///, " over a field ", TEX///$k$///},
	Ideal => "p" => {"a prime ideal ", TEX///$p \trianglelefteq k[x_1,\ldots,x_n]$///, " in the zero locus ", TEX///$V(f)$///},
	},
    Outputs => {
	GrothendieckWittClass => {"the class ", TEX///$\text{deg}_p^{\mathbb{A}^1}(f)$///, " in the Grothendieck-Witt ring ", TEX///$\text{GW}(k)$///}
	},
    PARA{"Given an endomorphism of affine space ", TEX///$f=(f_1,\dots ,f_n) \colon \mathbb{A}^n_k \to \mathbb{A}^n_k$///, " and an isolated zero ", TEX///$p\in V(f)$/// ,", we may compute its local ", TEX///$\mathbb{A}^1$///, EM "-Brouwer degree", " valued in the Grothendieck-Witt ring ", TEX///$\text{GW}(k)$///, "."
	},
    PARA{"For historical and mathematical background, see ", TO2(globalA1Degree, "global A1-degrees"), "."},
    EXAMPLE lines ///
    T1 = QQ[z_1..z_2];
    f1 = {(z_1-1)*z_1*z_2, (3/5)*z_1^2 - (17/3)*z_2^2};
    f1GD = globalA1Degree(f1);
    q=ideal {z_1,z_2};
    r=ideal {z_1-1,z_2^2-(9/85)};
    f1LDq= localA1Degree(f1,q)
    f1LDr= localA1Degree(f1,r)
    f1LDsum = gwAdd(f1LDq, f1LDr)
    ///,
    PARA{"The sum of the local A1-degrees is equal to the global A1-degree:"},
    EXAMPLE lines///
    gwIsomorphic(f1GD,f1LDsum)
    ///,
    SeeAlso => {"globalA1Degree", "sumDecomposition", "sumDecompositionString"}
    }




