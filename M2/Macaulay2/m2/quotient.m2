-- Copyright 1993-1999 by Daniel R. Grayson
-- Copyright 1996 by Michael E. Stillman

needs "max.m2" -- for infinity
needs "methods.m2"

-- ideal quotient methods moved to packages/Saturation.m2 in July 2020
quotient = method(
     Options => {
	  DegreeLimit       => {},
	  BasisElementLimit => infinity,
	  PairLimit         => infinity,
	  MinimalGenerators => true,
	  Strategy          => null
	  }
     )

-- moved to packages/Saturation.m2 in July 2020
saturate = method(Options => options quotient)

-- moved to packages/Saturation.m2 in October 2020
annihilator = method(Options => {Strategy => null}) -- Intersection or Quotient

gcdCoefficients(ZZ,ZZ) := (a,b) -> (
     m := {a,1,0};
     n := {b,0,1};
     if a<0 then m=-m;
     if b<0 then n=-n;
     if a>b then (k :=m;m=n;n=k);
     while m#0 > 0 do (
	  t := n#0 // m#0;
	  n = n - apply(m,y -> t * y);
	  (k=m;m=n;n=k);
	  );
     n);

mod = (i,n) -> i * 1_(ZZ/n)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
