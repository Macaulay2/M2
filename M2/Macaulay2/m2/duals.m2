-- Copyright 1995.  Michael E. Stillman

fromDual = method()
toDual = method()
fromDualChar0 = method()
dividedMultiply = method()
dividedPower = method()

fromDual Matrix := (f) -> (
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

toDual(ZZ, Matrix) := (d,f) -> (
    R := ring f;
    g := product apply(generators R, v -> v^d);
    box := matrix table (1, numgens R, (i,j) -> R_j^(d+1));
    contract(
	 transpose mingens image (generators (image box : image f) % box),
	 matrix{{g}}))


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
