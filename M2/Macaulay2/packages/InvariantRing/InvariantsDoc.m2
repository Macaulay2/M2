-*
   Copyright 2020, Luigi Ferraro, Federico Galetto,
   Francesca Gandini, Hang Huang, Matthew Mastroeni, Xianglong Ni.

   You may redistribute this file under the terms of the GNU General Public
   License as published by the Free Software Foundation, either version 2 of
   the License, or any later version.
*-

    
document {
	Key => {action, (action, RingOfInvariants)},
	
	Headline => "the group action that produced a ring of invariants",
	
	Usage => "action S",
	
	Inputs => {
	    	"S" => RingOfInvariants => {"of the group action being returned"},
		},
	
	Outputs => {
		GroupAction => {"the action that produced the ring of invariants in the input"}
		},
	"This function is provided by the package ", TO InvariantRing,".",
	
	PARA {
	    "This example shows how to recover the action of a
	    torus that produced a certain ring of invariants.
	    Note other action types are possible and may produce
	    a different looking output."
	    },
    	
	EXAMPLE {
		"R = QQ[x_1..x_4]",
		"T = diagonalAction(matrix {{0,1,-1,1},{1,0,-1,-1}}, R)",
		"S = R^T",
		"action S"
		},
	    }

document {
	Key => (generators, RingOfInvariants),
	
	Headline => "the generators for a ring of invariants",
	
	Usage => "generators S, gens S",
	
	Inputs => {
	    	"S" => RingOfInvariants,
		},
	    
	Outputs => {
		List => {"of algebra generators for the ring of invariants"}
		},
	    
	"This function is provided by the package ", TO InvariantRing,". ",
	
	PARA {
	    "This method gets the algebra generators for a ring of invariants."
	    },
    	
	EXAMPLE {
		"R = QQ[x_1..x_4]",
		"W = matrix{{0,1,-1,1},{1,0,-1,-1}}",
		"T = diagonalAction(W, R)",
		"S = R^T",
		"gens S",
		},
	    }

document {
	Key => {invariantRing, 
	    (invariantRing, GroupAction),
	    (symbol ^, PolynomialRing, GroupAction),
	    (symbol ^, QuotientRing, LinearlyReductiveAction)},
	
	Headline => "the ring of invariants of a group action",
	
	Usage => "invariantRing G, R^G",
	Inputs => {
	    	"G" => GroupAction,
	    	"R" => PolynomialRing => {"on which the group acts"},
		Strategy => {"the strategy used to compute the invariant ring"}
		},
	Outputs => {
		RingOfInvariants => {"the ring of invariants of the given group action"}
		},    
	Caveat => {
	    "By default, the invariants of a diagonal group action are computed over an infinite 
	    extension of the ground field specified by the user over which the action is defined.  
	    Provided the action is defined, it is possible to compute invariants literally over 
	    the specified ground field in prime characteristic using the option ", 
	    TO UseCoefficientRing, "."
	    },  
	    
	"This function is provided by the package ", TO InvariantRing,". ",
	
	PARA {
	    "The following example defines an action of a 
	    two-dimensional torus on a four-dimensional vector space
	    with a basis of weight vectors whose weights are
	    the columns of the input matrix."
	},
    	
	EXAMPLE {
		"R = QQ[x_1..x_4]",
		"W = matrix{{0,1,-1,1},{1,0,-1,-1}}",
		"T = diagonalAction(W, R)",
		"S = invariantRing T",
		},
	    
	PARA {
	    "The algebra generators for the ring of invariants are computed upon
	    initialization by the method ",
	    TO invariants,"."
	    },

	PARA {
	    "Alternatively, we can use the following shortcut to construct
	    a ring of invariants."
	    },
    	
	EXAMPLE {
		"S = R^T",
		},
	    }

document {
	Key => {
	    invariants
	    },
	
	Headline => "computes the generating invariants of a group action",
	
	Usage => "invariants G",
	
	Inputs => {  
	    	"G" => GroupAction => {"a specific type of group action on a polynomial ring"},
		Strategy => {"the strategy used to compute diagonal invariants, options are UsePolyhedra or UseNormaliz."}
		},
	Outputs => {
		"L" => List => {"a minimal set of generating invariants for the group action"}
		},

	PARA {
	    "This function is provided by the package ", TO InvariantRing, 
	    ". This function can be used to compute the generating invariants of a diagonal group action, finite group action or linearly reductive group action.
	    It can also be used to compute a basis of a graded component of the invariant ring. 
	    Below is a list of the many ways to use this function:"
	    },
	UL{
	    {TO (invariants, FiniteGroupAction), ": computes the generating invariants of a finite group action"},
	    {TO (invariants, DiagonalAction), ": computes the generating invariants of a diagonal group action"},
	    {TO (invariants, LinearlyReductiveAction), ": computes the generating invariants of a linearly reductive action"},
	    {TO (invariants, FiniteGroupAction, ZZ)," or ", TO (invariants, FiniteGroupAction, List), ": computes a basis for graded component of the invariant ring of a finite group action"},
	    {TO (invariants, LinearlyReductiveAction, ZZ)," or ", TO (invariants, LinearlyReductiveAction, List), ": computes a basis for graded component of the invariant ring of a linearly reductive group action"},
	    },
    
    	SeeAlso => {
	    invariantRing, 
	    isInvariant
	    },
	
	Caveat => {"Some optional inputs are only relevant to
	    certain use cases of this method.
	    Please consult the documentation pages for the
	    different cases to learn which optional inputs
	    are used."}
	}

document {
	Key => { 
	    (invariants, DiagonalAction)
	    },
	
	Headline => "computes the generating invariants of a group action",
	
	Usage => "invariants D",
	
	Inputs => {  
	    	"D" => DiagonalAction => {"a diagonal action on a polynomial ring"},
		Strategy => {"the strategy used to compute diagonal invariants, options are UsePolyhedra or UseNormaliz."}
		},
	Outputs => {
		"L" => List => {"a minimal set of generating invariants for the group action"}
		},
	Caveat => {
	    "By default, the invariants are computed over an infinite extension of the ground field
	    specified by the user over which the action is defined.  Provided the action is defined,
	    it is possible to compute invariants literally over the specified ground field in prime
	    characteristic using the option ", TO UseCoefficientRing, "."
	    }, 

	PARA {
	    "This function is provided by the package ", TO InvariantRing, 
	    ". It implements an algorithm to compute a minimal set of generating 
	    monomial invariants for a diagonal action of an abelian group",
	    TEX /// $(k^*)^r \times \mathbb{Z}/d_1 \times \cdots \times \mathbb{Z}/d_g$ ///,
	    " on a polynomial ring ",
	    TEX /// $R = k[x_1, \dots, x_n]$.///,
	    " Saying the action is diagonal means that ",
	    TEX /// $(t_1,\ldots,t_r) \in (k^*)^r$ ///,
	    " acts by",
	    TEX /// $$(t_1,\ldots,t_r) \cdot x_j = t_1^{w_{1,j}}\cdots t_r^{w_{r,j}} x_j$$ ///,
	    "for some integers ",
	    TEX /// $w_{i,j}$ ///, 
	    " and the generators ",
	    TEX /// $u_1, \dots, u_g$ ///,
	    " of the cyclic abelian factors act by",
	    TEX /// $$u_i \cdot x_j = \zeta_i^{w_{r+i,j}} x_j$$ ///,
	    "for ",
	    TEX /// $\zeta_i$ ///,
	    " a primitive ",
	    TEX /// $d_i$///,
	    "-th root of unity. The integers",
	    TEX /// $w_{i,j}$ ///,
	    "comprise the weight matrix ", TT "W",
	    ". In other words, the ",
	    TEX /// $j$ ///,
	    "-th column of ", TT "W", 
	    " is the weight vector of",
	    TEX /// $x_j$. ///
	    },
	 
	PARA {    
	    "The algorithm combines a modified version of an algorithm for tori 
	    due to Derksen and Kemper which can be found in: "
	    },
       
       UL { 
	    {"Derksen, H. & Kemper, G. (2015).", EM "Computational Invariant Theory", 
	   ". Heidelberg: Springer. pp 159-164"}
        },
    
       PARA {
	    "together with an algorithm for finite abelian groups due to Gandini 
	    which can be found in: "
	     },
	 
        UL { 
	    {"Gandini, F. ", EM "Ideals of Subspace Arrangements", 
	   ". Thesis (Ph.D.)-University of Michigan. 2019. ISBN: 978-1392-76291-2. pp 29-34."}
        },   
    
    	PARA {
	    "Here is an example of a one-dimensional torus acting on a 
	    two-dimensional vector space:"
	},
    
    	EXAMPLE {
	    	"R = QQ[x_1,x_2]",
		"W = matrix{{1,-1}}",
		"T = diagonalAction(W, R)",
		"invariants T"
		},
    
    	PARA {
	    "Here is an example of a product of two cyclic groups of 
	    order 3 acting on a three-dimensional vector space:"
	},
	
	EXAMPLE {
	    "R = QQ[x_1..x_3]",
	    "d = {3,3}",
	    "W = matrix{{1,0,1},{0,1,1}}",
	    "A = diagonalAction(W, d, R)",
	    "invariants A"
		},

    	PARA {
	    "Here is an example of a diagonal action by the product of
	     a two-dimensional torus with a cyclic group of order 3 
	    acting on a two-dimensional vector space:"
	},
    
	EXAMPLE {
	    "R = QQ[x_1, x_2]",
	    "d = {3}",
	    "W1 = matrix{{1,-1}, {-1,1}}",
	    "W2 = matrix {{1,0}}",
	    "D = diagonalAction(W1, W2, d, R)",
	    "invariants D"
		},
    
    	SeeAlso => {
	    diagonalAction,
	    invariantRing, 
	    isInvariant
	    }	
	}

document {
	Key => {
	    (invariants, FiniteGroupAction),
	    },
	Headline => "computes the generating invariants of a group action",
	Usage => "invariants G",
	Inputs => {
	    "G" => FiniteGroupAction,
	    Strategy => {"the strategy used to compute diagonal invariants, options are UsePolyhedra or UseNormaliz."}
	    },
	Outputs => {
		"L" => List => {"a minimal set of generating invariants for the group action"}
		},
	PARA {
	    "This function is provided by the package ", TO InvariantRing, "."
	    },
	PARA {
	    "It implements King's algorithm to compute a minimal
	    set of generating invariants for the action of a
	    finite group on a polynomial ring following Algorithm
	    3.8.2 in:"
	    },
       	UL { 
	    {"Derksen, H. & Kemper, G. (2015).",
		EM "Computational Invariant Theory", 
	   ". Heidelberg: Springer."}
        },
	PARA {
	    "The following example computes the invariants of the
	    alternating group on 4 elements."
	    },
    EXAMPLE {
	"R = QQ[x_1..x_4]",
	"L = apply({\"2314\",\"2143\"},permutationMatrix);",
	"A4 = finiteAction(L,R)",
	"netList invariants A4"
	},
    
    	SeeAlso => {
	    finiteAction,
	    invariantRing, 
	    isInvariant
	    }	
	}

document {
	Key => {
	    [invariants, DegreeBound], [invariantRing, DegreeBound], DegreeBound
	    },
	Headline => "degree bound for invariants of finite groups",
	Usage => "invariants G",
	Inputs => {"G" => FiniteGroupAction},
	Outputs => {
		"L" => List => {"a minimal set of generating invariants for the group action"}
		},
	PARA {
	    "This function is provided by the package ", TO InvariantRing, "."
	    },
	PARA {
	    "This optional argument allows the user to provide
	    an upper bound for the degree of the generating
	    invariants of a finite group action.
	    If no upper bound is provided, the order of the group
	    is used as an upper bound. Providing a smaller
	    upper bound may speed up the computation of invariants.
	    However, if the value provided is too small the
	    resulting list may not generate the ring of invariants."
	    },
	PARA {
	    "The following example computes the invariants of the
	    symmetric group on 4 elements."
	    },
    EXAMPLE {
	"R = QQ[x_1..x_4]",
	"L = apply({\"2134\",\"2341\"},permutationMatrix);",
	"S4 = finiteAction(L,R)",
	"elapsedTime invariants S4",
	"elapsedTime invariants(S4,DegreeBound=>4)"
	},
    	Caveat => {
	    "If the value provided for this option is too small,
	    then the output list does not generate
	    the entire ring of invariants. A warning message
	    is produced to notify the user of the issue."
	    },
    	SeeAlso => {
	    finiteAction,
	    invariantRing, 
	    isInvariant
	    }	
	}

document {
	Key => {
	    [invariants, UseCoefficientRing], [invariantRing, UseCoefficientRing], UseCoefficientRing
	    },
	Headline => "option to compute invariants over the given coefficient ring",
	Usage => "invariants G",
	Inputs => {"D" => DiagonalAction},
	Outputs => {
		"L" => List => {"a minimal set of generating invariants for the group action"}
		},
	
	PARA {
	    "This function is provided by the package ", TO InvariantRing, "."
	    },
	
	PARA {
	    "By default, the invariants of a diagonal action are
	    computed over an infinite extension of the coefficient
	    field specified by the user over which the action is defined.
	    Setting this optional argument to ", TO true, " will compute
	    the invariants of the action literally over the finite field
	    specified by the user in prime characteristic, provided the 
	    action is defined."
	    },
	
	PARA {
	    "The following example computes the invariants of a
	    1-dimensional torus action literally over the specified finite 
	    field."
	    },
	
	EXAMPLE {
	    "R = (GF 9)[x, y]",
	    "W = matrix {{7, -5}}",
	    "T = diagonalAction(W, R)",
	    "invariantRing(T,UseCoefficientRing => true)"
	    },
	
	PARA {
	    "Over an infinite extension of the given ground field, there
	    are fewer invariants."
	    },
	
	EXAMPLE {
	    "invariantRing T"
	    },
    
    	SeeAlso => {
	    diagonalAction,
	    invariants,
	    invariantRing
	    }	
	}

document {
	Key => {
	    [invariants, UseLinearAlgebra], [invariantRing, UseLinearAlgebra], UseLinearAlgebra
	    },
	Headline => "strategy for computing invariants of finite groups",
	Usage => "invariants G",
	Inputs => {"G" => FiniteGroupAction},
	Outputs => {
		"L" => List => {"a minimal set of generating invariants for the group action"}
		},
	
	PARA {
	    "This function is provided by the package ", TO InvariantRing, "."
	    },
	
	PARA {
	    "This optional argument determines the strategy used to
	    compute generating invariants of a finite group action.
	    The default strategy uses the Reynolds operator, however
	    this may be slow for large groups. Setting this argument
	    to ", TO true, " uses the linear algebra method for
	    computing invariants of a given degree by calling ",
	    TO (invariants, FiniteGroupAction, ZZ), ". This may
	    provide a speedup at lower degrees, especially if the
	    user-provided generating set for the group is small."
	    },
	
	PARA {
	    "The following example computes the invariants of the
	    symmetric group on 4 elements. Note that using
	    different strategies may lead to different sets of 
	    generating invariants."
	    },
	
	EXAMPLE {
	    "R = QQ[x_1..x_4]",
	    "L = apply({\"2134\",\"2341\"},permutationMatrix);",
	    "S4 = finiteAction(L,R)",
	    "elapsedTime invariants S4",
	    "elapsedTime invariants(S4,UseLinearAlgebra=>true)"
	},
    
    	SeeAlso => {
	    finiteAction,
	    invariantRing, 
	    isInvariant
	    }	
	}

document {
	Key => {
	    (invariants, FiniteGroupAction, ZZ),
	    (invariants, FiniteGroupAction, List),
	    },
	
	Headline => "basis for graded component of invariant ring",
	
	Usage => "invariants(G,d)",
	
	Inputs => {  
	    	"G" => FiniteGroupAction,
		"d" => ZZ => {"a degree or multidegree"},
	    	Strategy => {"the strategy used to compute diagonal invariants, options are UsePolyhedra or UseNormaliz."}
		},
	Outputs => {
		"L" => List => {"an additive basis for a graded component of the ring of invariants"}
		},

	PARA {
	    "This function is provided by the package ", TO InvariantRing,
	    },
	
	PARA {
	    "When called on a finite group action and
	    a (multi)degree, it computes an additive basis for the
	    invariants of the action in the given degree."},
	PARA {
	    "This function
	    uses an implementation of the Linear Algebra Method
	    described in §3.1.1 of"
	    },
       	UL { 
	    {"Derksen, H. & Kemper, G. (2015).", EM "Computational Invariant Theory", 
	   ". Heidelberg: Springer."}
        },
	PARA { "For example, consider the following
	    action of a dihedral group."
	    },
    	EXAMPLE {
	    	"K=toField(QQ[a]/(a^2+a+1));",
	    	"R = K[x,y]",
		"A=matrix{{a,0},{0,a^2}};",
		"B=sub(matrix{{0,1},{1,0}},K);",
		"D6=finiteAction({A,B},R)",
		"invariants(D6,20)",
		},
	PARA { "It is important to note that this implementation
	    uses the group generators provided by the user,
	    which can be recovered using ", TO (gens,FiniteGroupAction),
	    ". To improve efficiency the user should provide
	    a generating set for the group that is as small as
	    possible."
	    },
	   
    	SeeAlso => {
	    invariantRing, 
	    isInvariant,
	    finiteAction
	    }
	}

document {
	Key => {
	    (invariants, LinearlyReductiveAction, ZZ),
	    (invariants, LinearlyReductiveAction, List)
	    },
	
	Headline => "basis for graded component of invariant ring",
	
	Usage => "invariants(V,d)",
	
	Inputs => {  
	    	"V" => LinearlyReductiveAction,
		"d" => ZZ => {"a degree or multidegree"},
	    	Strategy => {"the strategy used to compute diagonal invariants, options are UsePolyhedra or UseNormaliz."}
		},
	Outputs => {
		"L" => List => {"an additive basis for a graded component of the ring of invariants"}
		},

	PARA {
	    "This function is provided by the package ", TO InvariantRing,
	    },
	
	PARA {
	    "When called on a linearly reductive group action and
	    a (multi)degree, it computes an additive basis for the
	    invariants of the action in the given degree."},
	PARA {
	    "This function uses an implementation of Algorithm
	    4.5.1 in:"
	    },
       	UL { 
	    {"Derksen, H. & Kemper, G. (2015).", EM "Computational Invariant Theory", 
	   ". Heidelberg: Springer."}
        },
    	PARA {
	    "The following example examines the action of the
	    special linear group of degree 2 on the space of
	    binary quadrics. There is a single invariant of degree
	    2 but no invariant of degree 3."
	    },
    	EXAMPLE {
	    	"S = QQ[a,b,c,d]",
		"I = ideal(a*d - b*c - 1)",
		"A = S[u,v]",
		"M = transpose (map(S,A)) last coefficients sub(basis(2,A),{u=>a*u+b*v,v=>c*u+d*v})",
		"R = QQ[x_1..x_3]",
		"V = linearlyReductiveAction(I,M,R)",
		"invariants(V,2)",
		"invariants(V,3)",
		},
	   
    	SeeAlso => {
	    invariantRing, 
	    isInvariant,
	    linearlyReductiveAction
	    }
	}

document {
	Key => {
	    (invariants, LinearlyReductiveAction)
	    },
	
	Headline => "invariant generators of Hilbert ideal",
	
	Usage => "invariants V",
	
	Inputs => {  
	    	"V" => LinearlyReductiveAction,
	    	Strategy => {"the strategy used to compute diagonal invariants, options are UsePolyhedra or UseNormaliz."}
		},
	Outputs => {
		"L" => List => {"of invariants generating the Hilbert ideal"}
		},

	PARA {
	    "This function is provided by the package ", TO InvariantRing,
	    },
	
	PARA {
	    "When called on a linearly reductive group action and
	    a degree, this function returns a list of generators for the
	    Hilbert ideal that are also invariant under the action.
	    This function computes the Hilbert ideal first using ",
	    TO "hilbertIdeal", " then finds invariant generators
	    degree by degree using ",
	    TO "invariants(LinearlyReductiveAction,ZZ)", ".",
	    },
	
    	PARA {
	    "The next example constructs a cyclic group of order 2
	    as a set of two affine points. Then it introduces an
	    action of this group on a polynomial ring in two variables
	    and computes the Hilbert ideal. The action permutes the
	    variables of the polynomial ring."
	    },
	
    	EXAMPLE {
		"S = QQ[z]",
		"I = ideal(z^2 - 1)",
		"M = matrix{{(z+1)/2, (1-z)/2},{(1-z)/2, (z+1)/2}}",
		"sub(M,z=>1),sub(M,z=>-1)",
		"R = QQ[x,y]",
		"V = linearlyReductiveAction(I, M, R)",
		"H = hilbertIdeal V",
		"invariants V",
		},
	PARA {
	    "The algorithm for the Hilbert ideal performs an
	    elimination using Groebner
	    bases. The options ", TO DegreeLimit, " and ",
	    TO SubringLimit, " are standard ", TO gb, " options
	    that can be used to interrupt the computation
	    before it is complete, yielding a partial list of
	    invariant generators for the Hilbert ideal."
	    },
	    
	Caveat => {
	    "Both ", TO "hilbertIdeal", " and ",
	    TO "invariants(LinearlyReductiveAction,ZZ)",
	    " require Groebner bases computations, which could
	    lead to long running times. It might be helpful to
	    run these functions separately.",
	    },
	
    	SeeAlso => {
	    hilbertIdeal, 
	    invariants
	    },
	}

document {
	Key => {
	    [invariants, DegreeLimit],
	    [hilbertIdeal, DegreeLimit]
	    },
	Headline => "GB option for invariants",
	PARA {
	    "The computation of invariants of linearly reductive
	    group actions requires the use of Gröbner bases.
	    These options allow partial control over the computation
	    performed by ", TO (invariants,LinearlyReductiveAction),
	    " and ", TO (hilbertIdeal,LinearlyReductiveAction),
	    ", allowing to terminate the computation after
	    reaching a certain degree. For more information,
	    see ", TO gb, "."
	    },
    	SeeAlso => {
	    (invariants,LinearlyReductiveAction),
	    (hilbertIdeal,LinearlyReductiveAction)
	    }	
	}

document {
	Key => {
	    [invariants, SubringLimit],
	    [hilbertIdeal, SubringLimit]
	    },
	Headline => "GB option for invariants",
	PARA {
	    "The computation of invariants of linearly reductive
	    group actions requires the use of Gröbner bases.
	    These options allow partial control over the computation
	    performed by ", TO (invariants,LinearlyReductiveAction),
	    " and ", TO (hilbertIdeal,LinearlyReductiveAction),
	    ", allowing to terminate the computation after a
	    certain number of invariants are obtained.
	    For more information,
	    see ", TO gb, "."
	    },
    	SeeAlso => {
	    (invariants,LinearlyReductiveAction),
	    (hilbertIdeal,LinearlyReductiveAction)
	    }	
	}

document {
	Key => {isInvariant, 
	    (isInvariant, RingElement, FiniteGroupAction),
	    (isInvariant, RingElement, DiagonalAction),
	    (isInvariant, RingElement, LinearlyReductiveAction)
	    },
	
	Headline => "check whether a polynomial is invariant under a group action",
	Usage => "isInvariant(f, G), isInvariant(f, D), isInvariant(f, L)",
	Inputs => {
	    	"f" => RingElement => {"a polynomial in the polynomial ring on which the group acts"},
	    	"G" => FiniteGroupAction,
		"D" => DiagonalAction,
		"L" => LinearlyReductiveAction
		},
	    
	Outputs => {
		Boolean => "whether the given polynomial is invariant under 
		the given group action"
		},
	    
	"This function is provided by the package ", TO InvariantRing,". ",
    	
	PARA {
	    "This function checks if a polynomial is invariant
	    under a certain group action."
	    },
	
	PARA {
	    "The following example defines the permutation action
	    of a symmetric group on a polynomial ring with three
	    variables."
	    },
	
	EXAMPLE {
	    "R = QQ[x_1..x_3]",
	    "L = apply(2, i -> permutationMatrix(3, [i + 1, i + 2] ) )",
	    "S3 = finiteAction(L, R)",
	    "isInvariant(1 + x_1^2 + x_2^2 + x_3^2, S3)",
	    "isInvariant(x_1*x_2*x_3^2, S3)"
		},
    
    	PARA {
	    "Here is an example with a two-dimensional torus
	    acting on polynomial ring in four variables:"
	    },
	
	EXAMPLE {
	    "R = QQ[x_1..x_4]",
	    "W = matrix{{0,1,-1,1}, {1,0,-1,-1}}",
	    "T = diagonalAction(W, R)",
	    "isInvariant(x_1*x_2*x_3, T)",
	    "isInvariant(x_1*x_2*x_4, T)"
		},
	    
         PARA {
	    "Here is another example of a product of two cyclic groups
	    of order 3 acting on a three-dimensional vector space:"
	    },
	
	EXAMPLE {
	    "R = QQ[x_1..x_3]",
	    "W = matrix{{1,0,1}, {0,1,1}}",
	    "A = diagonalAction(W, {3,3}, R)",
	    "isInvariant(x_1*x_2*x_3, A)",
	    "isInvariant((x_1*x_2*x_3)^3, A)"
		},

         PARA {
	    "Here is an example with a general linear group
	    acting by conjugation on a space of matrices
	    (determinant and trace are invariants)."
	    },
	
	EXAMPLE {
	    "S = QQ[a,b,c,d,t]",
	    "I = ideal((det genericMatrix(S,2,2))*t-1)",
	    "R = QQ[x_(1,1)..x_(2,2)]",
	    "Q = (S/I)(monoid R);",
	    "G = transpose genericMatrix(S/I,2,2)",
	    "X = transpose genericMatrix(Q,x_(1,1),2,2)",
	    "N = reshape(Q^1,Q^4,transpose(inverse(G)*X*G));",
	    "phi = map(S,Q);",
	    "M = phi last coefficients N;",
	    "L = linearlyReductiveAction(I, M, R)",
	    "isInvariant(det genericMatrix(R,2,2),L)",
	    "isInvariant(trace genericMatrix(R,2,2),L)"
		}

	    }	

document {
	Key => {reynoldsOperator, (reynoldsOperator, RingElement, FiniteGroupAction),(reynoldsOperator, RingElement, DiagonalAction)},
	
	Headline => "the image of a polynomial under the Reynolds operator",
	
	Usage => "reynoldsOperator(f, G), reynoldsOperator(f, D)",
	
	Inputs => {
	    	"f" => RingElement => {"a polynomial in the polynomial ring of the given group action"},
	    	"G" => FiniteGroupAction,
		"D" => DiagonalAction
		},
	    
	Outputs => {
		RingElement => {"the invariant polynomial which is the image of the given 
		    polynomial under the Reynolds operator of the given finite group action or the given torus action"}
		},
	    
	"This function is provided by the package ", TO InvariantRing,". ",
	
	PARA {
	    "The following example computes the image of a polynomial under the
	    Reynolds operator for a cyclic permutation of the variables."
	    },
    	
	EXAMPLE {
	    "R = ZZ/3[x_0..x_6]",
	    "P = permutationMatrix toString 2345671",
	    "C7 = finiteAction(P, R)",
	    "reynoldsOperator(x_0*x_1*x_2^2, C7)",
		},
       PARA {
	    "Here is an example computing the image of a polynomial under the Reynolds operator for a two-dimensional torus
	    acting on polynomial ring in four variables:"
	    },
	
	EXAMPLE {
	    "R = QQ[x_1..x_4]",
	    "W = matrix{{0,1,-1,1}, {1,0,-1,-1}}",
	    "T = diagonalAction(W, R)",
	    "reynoldsOperator(x_1*x_2*x_3 + x_1*x_2*x_4, T)",
		},
	        
	
	    }

document {
	Key => {definingIdeal,
	     (definingIdeal, RingOfInvariants)},
	
	Headline => "presentation of a ring of invariants as polynomial ring modulo the defining ideal",
	
	Usage => "definingIdeal S",
	
	Inputs => {
	    	"S" => RingOfInvariants,
		Variable => "name of the variables in the polynomial ring."
		},
	    
	Outputs => {
		Ideal => {"which defines the ring of invariants as a polynomial ring modulo the ideal."}
		},
	    
	"This function is provided by the package ", TO InvariantRing,". ",
	
	PARA {
	    "This method presents the ring of invariants as a polynomial ring modulo the defining ideal. The default variable name in the polynomial ring is ",TT "u_i",". You can pass the variable name you want as optional input."
	    },
    	
	EXAMPLE {
		"R = QQ[x_1..x_4]",
		"W = matrix{{0,1,-1,1},{1,0,-1,-1}}",
		"T = diagonalAction(W, R)",
		"S = R^T",
		"definingIdeal S",
		},
	    }
	
	
document {
	Key => {RingOfInvariants},
	
	Headline => "the class of the rings of invariants under the action of a finite group, an Abelian group or a linearly reductive group",
	
	"This class is provided by the package ", TO InvariantRing,".",
	
	PARA {
	    	TT "RingOfInvariants", " is the class of rings of invariants when a finite group, an Abelian group or a linearly reductive group acts on a polynomial ring."
	    },
	}
    
    
document {
	Key => {(ambient, RingOfInvariants)},
	
	Headline => "the ambient polynomial ring where the group acted upon",
	
	Usage => "ambient S",
	
	Inputs => {
	    	"S" => RingOfInvariants => {"of the group action being returned"},
		},
	
	Outputs => {
		PolynomialRing => {"where the group acted upon"}
		},
	"This function is provided by the package ", TO InvariantRing,".",
	
	PARA {
	    "This example shows how to recover the polynomial ring when a torus acted upon."
	    },
    	
	EXAMPLE {
		"R = QQ[x_1..x_4]",
		"T = diagonalAction(matrix {{0,1,-1,1},{1,0,-1,-1}}, R)",
		"S = R^T",
		"ambient S"
		},
	    }
	
document {
	Key => {(hilbertSeries, RingOfInvariants)},
	
	Headline => "Hilbert series of the invariant ring",
	
	Usage => "hilbertSeries S",
	
	Inputs => {
	    	"S" => RingOfInvariants,
		},
	    
	Outputs => {
		Divide => {"the Hilbert series of the invariant ring as a module over the ambient polynomial ring."}
		},
	    
	"This function is provided by the package ", TO InvariantRing,". ",
	
	PARA {
	    "This method computes the hilbert series of the ring of invariants."
	    },
    	
	EXAMPLE {
		"R = QQ[x_1..x_4]",
		"W = matrix{{0,1,-1,1},{1,0,-1,-1}}",
		"T = diagonalAction(W, R)",
		"S = R^T",
		"hilbertSeries S",
		},
	    }

document {
	Key => {UseNormaliz, UsePolyhedra},
	Headline => "option for diagonal invariants",
	"This option is provided by the package ", TO InvariantRing,". ",
	PARA {
	    "The computation of diagonal invariants relies on
	    finding integral points in a convex hull constructed
	    from a weight matrix. This option selects the package
	    used for finding integral points. See ",
	    TO (invariants,DiagonalAction),
	    " for usage."
	    },
	}
