-- Copyright 1995.  Michael E. Stillman

fromDual = (f) -> (
    R := ring f;
    d := first max degrees source f;
    g := product apply(generators R, v -> v^d);
    f1 := contract(transpose f, matrix{{g}});
    mingens (
	 image (target f1 ** matrix table(1, numgens R, (i,j) -> R_j^(d+1))) 
	 : 
	 image f1
	 )
    )

toDual = (d, f) -> (
    R := ring f;
    g := product apply(generators R, v -> v^d);
    box := matrix table (1, numgens R, (i,j) -> R_j^(d+1));
    contract(
	 transpose mingens image (generators (image box : image f) % box),
	 matrix{{g}}))

TEST "
    R = ZZ/101[a..d]
    f = matrix{{a^3 + b^3 + c^3 + d^3 + (a+b+c)^3}}
    fdual = fromDual f
    assert(f - toDual(4, fdual) == 0)
"

document { quote fromDual,
     TT "fromDual f", " -- given a 1 by n matrix ", TT "f", " over a polynomial
     ring R, computes g so that ", TT "Hom(R/image g, E)", " corresponds to
     f, where E is the injective envelope of the coefficient field of R.",
     PARA,
     "This function mimics the script ", TT "<l_from_dual", " of Macaulay, and
     is probably going to be changed substantially.",
     SEEALSO "toDual"
     }

document { quote toDual,
     TT "toDual(d,f)", " -- given a 1 by n matrix ", TT "f", " over a polynomial
     ring R and an integer d such that the d-th power of each variable is in
     the image of f, computes ", TT "Hom(R/image f, E)", ", where
     E is the injective envelope of the coefficient field of R.",
     PARA,
     "This function mimics the script ", TT "<l_to_dual", " of Macaulay, and
     is probably going to be changed substantially.",
     SEEALSO "fromDual"
     }
