-- -*- coding: utf-8 -*-
newPackage(
     "PrimaryDecomposition",
     AuxiliaryFiles => true,
     Headline => "functions for primary decomposition (pre-loaded)"
     )
export {
     EisenbudHunekeVasconcelos,					    -- cryptic
     Hybrid,
     Increment,
     GTZ,
     ShimoyamaYokoyama,
--     binomialCD,
--     extract,
--     findNonMember,
--     flattener,
     localize,
--     minSat,
     primaryComponent
--     quotMin,
--     radicalContainment
     }


--     EHVprimaryDecomposition,			    -- cryptic
--     HprimaryDecomposition,
--     Hybrid,
--     primdecComputation,
--     minSatPPD,
--     sortByDegree

load "PrimaryDecomposition/GTZ.m2"
load "PrimaryDecomposition/Shimoyama-Yokoyama.m2"
load "PrimaryDecomposition/Eisenbud-Huneke-Vasconcelos.m2"

binomialCD = (I) -> error "Binomial strategy not implemented yet"

Hybrid = new SelfInitializingType of BasicList
///
primaryDecomposition Ideal := List => o -> (I) -> (
     -- Determine the strategy to use.
     opt := ShimoyamaYokoyama;
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
	  I.cache#"AssociatedPrimes" = apply(C, I -> ideal radical I);
	  C/ideal
	  )
     else if opt === Binomial then binomialCD I
     else if opt === EisenbudHunekeVasconcelos then EHVprimaryDecomposition I
     else if opt === ShimoyamaYokoyama then SYprimaryDecomposition I
     else if class opt === Hybrid then (
	  if #opt =!= 2 then error "the Hybrid strategy requires 2 arguments";
	  assStrategy := opt#0;
	  localizeStrategy := opt#1;
	  HprimaryDecomposition ( I, assStrategy, localizeStrategy )
	  )
     )
///
primedecomp = (I,strategy) -> (
     -- Determine the strategy to use.
     opt := ShimoyamaYokoyama;
     if strategy =!= null then (
	  opt = strategy;
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
	  I.cache#"AssociatedPrimes" = apply(C, I -> ideal radical I);
	  C/ideal
	  )
     else if opt === Binomial then binomialCD I
     else if opt === EisenbudHunekeVasconcelos then EHVprimaryDecomposition I
     else if opt === ShimoyamaYokoyama then SYprimaryDecomposition I
     else if class opt === Hybrid then (
	  if #opt =!= 2 then error "the Hybrid strategy requires 2 arguments";
	  assStrategy := opt#0;
	  localizeStrategy := opt#1;
	  HprimaryDecomposition ( I, assStrategy, localizeStrategy )
	  )
     )

primaryDecomposition Ideal := List => o -> (J) -> (
     R := ring J;
     if isPolynomialRing R 
     then primedecomp(J, o.Strategy)
     else (
	  (A,F) := flattenRing R;
	  G := F^-1;
	  B := ring presentation A;
	  J' := lift(F J,B);
	  -- if B is suitable, then B1 is not needed
	  B1 := (coefficientRing B)[gens B];
	  toB1 := map(B1,B,vars B1);
	  fromB1 := map(B,B1,vars B);
	  J' = toB1 J';
	  C := primedecomp(J', o.Strategy);
	  J.cache#"AssociatedPrimes" = apply(associatedPrimes J', P -> trim G promote(fromB1 P,A));
	  apply(C, Q -> trim G promote(fromB1 Q,A))
	  ))

beginDocumentation()

document {
     Key => PrimaryDecomposition,
     "This package provides computations with components
     of ideals, including minimal and associated primes, radicals, and
     primary decompositions of ideals.",
     Subnodes => {
	  TO (associatedPrimes, Ideal),
	  TO [associatedPrimes,Strategy],
	  TO (localize,Ideal,Ideal),
	  TO [localize,Strategy],
	  TO (primaryComponent, Ideal, Ideal),
	  TO [primaryComponent,Strategy],
	  TO [primaryComponent,Increment],
	  -- this is part of the Macaulay2 package, sorry! TO (primaryDecomposition, Ideal),
	  TO [primaryDecomposition,Strategy]
	  },
     SeeAlso => { (primaryDecomposition, Ideal) }
     }

load "PrimaryDecomposition/doc.m2"

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=PrimaryDecomposition pre-install"
-- End:
