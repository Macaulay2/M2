--		Copyright 1993-1999 by Daniel R. Grayson

-- This definition is used in several places, so it doesn't have
-- a natural home.  It used to be in methods.m2, but
-- 'infinity' isn't defined yet when reading that file.

assert( class infinity === InfiniteNumber )

quotient = method(
     Options => {
	  DegreeLimit => {},
	  BasisElementLimit => infinity,
	  PairLimit => infinity,
	  MinimalGenerators => true,
	  Strategy => null
	  }
     )

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
