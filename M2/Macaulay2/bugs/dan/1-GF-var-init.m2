the variable should be in F

    i1 : F = GF(81,Variable=>a)

    o1 = F

    o1 : GaloisField

    i2 : a

    o2 = a

	 ZZ
    o2 : -- [a]
	  3

This is a side effect of making Galois fields have an empty list of vars.

Just add an assignment to the "use" for F.

Also, we should negate the assignment to variables for the 1-var poly ring used by GF.
