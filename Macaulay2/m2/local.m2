--		Copyright 1993-1998 by Daniel R. Grayson, Michael E. Stillman

-- local cohomology

truncatedDual := (M,e) -> (
     -- find (k-dual M), truncated in degrees >= e.
     R := ring M;
     n := numgens R;
     ww := R^{-n};
     M1 := prune (M / (truncate(-e+1,M)));
     Ext^n(M1,ww))

cohomology(ZZ,Module) := Module => opts -> (i,M) -> (
     -- this is local cohomology for the maximal ideal
     e := opts.Degree;
     if e == -infinity then error "not implemented yet";
     A := ring M;
     if not isAffineRing A then error "expected a module over an affine ring";
     F := presentation A;
     R := ring F;
     M = coker lift(presentation M,R) ** coker F;
     n := numgens R;
     ww := R^{-n};
     E := prune Ext^(n-i)(M,ww);
     result := if dim E <= 0 then Ext^n(E,ww) else truncatedDual(E,e);
     prune (result ** A)
     )

