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
-- used in matrix2.m2
quotient' = method(Options => options quotient)

-- moved to packages/Saturation.m2 in July 2020
saturate = method(Options => options quotient)

-- moved to packages/Saturation.m2 in October 2020
annihilator = method(Options => {Strategy => null}) -- Intersection or Quotient

gcdCoefficients(ZZ,ZZ) := gcdCoefficients0

mod = (i,n) -> i * 1_(ZZ/n)

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
