--load "primdecomp-SY.m2"
--load "primdecomp-EHV.m2"

binomialCD = (I) -> {}

Hybrid = new SelfInitializingType of BasicList

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
	  primaryDecomposition monomialIdeal I
	  )
     else if opt === Binomial then (
	  error "not implemented yet";
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


     
















-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/m2 "
-- End:
