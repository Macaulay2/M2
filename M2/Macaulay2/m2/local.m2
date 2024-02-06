--		Copyright 1993-1998 by Daniel R. Grayson, Michael E. Stillman

needs "ext.m2"

-- local cohomology

truncatedDual := (M,e) -> (
     -- find (k-dual M), truncated in degrees >= e.
     -- depends on truncate methods
     needsPackage "Truncations";
     R := ring M;
     n := numgens R;
     ww := R^{-n};
     M1 := minimalPresentation (M / (truncate(-e+1,M)));
     Ext^n(M1,ww))

cohomology(ZZ,Module) := Module => opts -> (i,M) -> (
     -- this is local cohomology for the maximal ideal
     e := opts.Degree;
     if e == -infinity then error "not implemented yet";
     A := ring M;
     if not isAffineRing A then error "expected a module over an affine ring";
     F := presentation A;
     R := ring F;
     M = cokernel lift(presentation M,R) ** cokernel F;
     n := numgens R;
     ww := R^{-n};
     E := minimalPresentation Ext^(n-i)(M,ww);
     result := if dim E <= 0 then Ext^n(E,ww) else truncatedDual(E,e);
     minimalPresentation (result ** A)
     )


-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
