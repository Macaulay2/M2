newPackage(
     "PrimaryDecomposition",
     Headline => "functions for primary decomposition (pre-loaded)"
     )
export (
     EHV,					    -- cryptic
     binomialCD,
     extract,
     findNonMember,
     flattener,
     localize,
     minSat,
     primaryComponent,
     quotMin,
     radicalContainment
     )

--     EHVprimaryDecomposition,			    -- cryptic
--     HprimaryDecomposition,
--     Hybrid,
--     primdecComputation,
--     minSatPPD,
--     sortByDegree

load "PrimaryDecomposition/GTZ.m2"
load "PrimaryDecomposition/Shimoyama-Yokoyama.m2"
load "PrimaryDecomposition/Eisenbud-Huneke-Vasconcelos.m2"

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
	  C := primaryDecomposition monomialIdeal I;
	  I.cache.Assassinator = apply(C, I -> ideal radical I);
	  C/ideal
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

beginDocumentation()

document {
     Key => PrimaryDecomposition,
     "This package provides computations with components
     of ideals, including minimal and associated primes, radicals, and
     primary decompositions of ideals.",
     PARA,
     Subnodes => {
	  TO (ass, Ideal),
	  TO [ass,Strategy],
	  TO [ass,PrintLevel],
	  TO (localize,Ideal,Ideal),
	  TO [localize,Strategy],
	  TO [localize,PrintLevel],
	  TO (primaryComponent, Ideal, Ideal),
	  TO [primaryComponent,Strategy],
	  TO [primaryComponent,Increment],
	  TO [primaryComponent,PrintLevel],
	  TO (primaryDecomposition, Ideal),
	  TO [primaryDecomposition,Strategy]
	  }
     }

load "PrimaryDecomposition/doc.m2"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PrimaryDecomposition.installed"
-- End:
