--load "primdecomp-SY.m2"
--load "primdecomp-EHV.m2"

isMonomialIdeal = (I) -> false
monomialIdealPD = (I) -> {}
binomialCD = (I) -> {}
--EHVprimaryDecomposition = (I,printlevel) -> {}
--SYprimaryDecomposition = (I,printlevel) -> {}
--HprimaryDecomposition = (I,printlevel) -> {}

Hybrid = new SelfInitializingType of BasicList

-- This is just used for testing...
clear = method()
clear Ideal := (I) -> ideal clear gens I
clear Matrix := (m) -> matrix entries m



primaryDecomposition = method(
     Options => {
	  PrintLevel => 0,
	  Strategy => null
	  }
     )

primaryDecomposition Ideal := List => o -> (I) -> (
     -- Determine the strategy to use.
     opt := SY;
     if o.Strategy =!= null then (
	  opt = o.Strategy;
	  if opt === Monomial and not isMonomialIdeal I
	  then error "cannot use 'Monomial' strategy on non monomial ideal";
	  )
     else (
	  -- if we have a monomial ideal: use Monomial
	  if isMonomialIdeal I then 
	     opt = Monomial;
	  );
     -- Now call the correct algorithm
     if opt === Monomial then (
	  monomialIdealPD I
	  )
     else if opt === Binomial then (
	  binomialCD (I,o.PrintLevel)
	  )
     else if opt === EHV then (
	  EHVprimaryDecomposition (I,o.PrintLevel)
	  )
     else if opt === SY then (
	  SYprimaryDecomposition (I,o.PrintLevel)
	  )
     else if class opt === Hybrid then (
	  if #opt =!= 2 then error "hybrid requires 2 arguments";
	  assStrategy := opt#0;
	  localizeStrategy := opt#1;
	  HprimaryDecomposition (
	       I,
	       assStrategy,
	       localizeStrategy,
	       o.PrintLevel)
	  )
     )


     















