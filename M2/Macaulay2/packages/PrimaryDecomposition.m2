---------------------------------------------------------------------------
-- PURPOSE : Computation of quotient, saturation, and annihilator
--
-- UPDATE HISTORY : created pre-2000 EHV algorithm for associated primes &
--                                    SY algorithm for primary decomposition
--                  updated Feb 2005
--                  updated Nov 2006
--                  updated Mar 2007 GTZ algorithm expanded
--                  updated Jan 2009 support for towers of rings and newGTZ
--                  updated May 2015 more testing added
--                  updated Nov 2020
--
-- TODO : 1.
---------------------------------------------------------------------------
newPackage(
    "PrimaryDecomposition",
    Version => "2.0",
    Date => "July 4, 2020",
    Headline => "primary decomposition routines",
    Authors => {
	{Name => "Mike Stillman",  Email => "mike@math.cornell.edu", HomePage => "http://www.math.cornell.edu/~mike"},
	{Name => "Carolyn Yackel", Email => "cyackel@math.indiana.edu"},
	{Name => "Justin Chen",    Email => "justin.chen@math.gatech.edu"},
	{Name => "Mahrud Sayrafi", Email => "mahrud@umn.edu",        HomePage => "https://math.umn.edu/~mahrud"}},
    Keywords => {"Commutative Algebra"},
    PackageExports => {"Colon"},
    AuxiliaryFiles => true,
    DebuggingMode => true
    )

export {
    -- methods
    "primaryDecomposition",
    "irreducibleDecomposition",
    "isPrimary",
    -- keys for strategies
    "EisenbudHunekeVasconcelos",					    -- cryptic
    "Hybrid",
    "Increment",
    "GTZ",
    "ShimoyamaYokoyama",
--     "binomialCD",
--     "extract",
--     "findNonMember",
--     "flattener",
    "localize",
--     "minSat",
    "primaryComponent",
--     "quotMin",
    "kernelOfLocalization",
    "regSeqInIdeal",
    "radicalContainment"
    }

importFrom_Core { "raw", "rawIndices" }

-- private symbols used as keys:

protect H, protect U, protect W

--     EHVprimaryDecomposition,			    -- cryptic
--     HprimaryDecomposition,
--     Hybrid,
--     primdecComputation,
--     minSatPPD,
--     sortByDegree

--------------------------------------------------------------------
-- Support routines
--------------------------------------------------------------------

primaryDecomposition = method( TypicalValue => List, Options => { Strategy => null } )

--  ASSOCIATED PRIMES  -------------------------------------
ass0 := (I) -> (
     if I.cache#?associatedPrimes
     then I.cache#associatedPrimes
     else I.cache#associatedPrimes = (
     	  R := ring I;
     	  J := dual I;
     	  M := first entries generators J;
	  H := new MutableHashTable;
     	  scan(M, m -> (
		    s := rawIndices raw m;
		    if not H#?s then H#s = true));
	  inds := sort apply(keys H, ind -> (#ind, ind));
	  apply(inds, s -> s#1)
     ))

-- TODO: move exported methods out of the files below
load "./PrimaryDecomposition/radical.m2"
load "./PrimaryDecomposition/GTZ.m2"
load "./PrimaryDecomposition/Shimoyama-Yokoyama.m2"
load "./PrimaryDecomposition/Eisenbud-Huneke-Vasconcelos.m2"

binomialCD = (I) -> error "Binomial strategy not implemented yet"

Hybrid = new SelfInitializingType of BasicList

--------------------------------------------------------------------
-- isPrimary
--------------------------------------------------------------------

isPrimary = method()
isPrimary(Ideal) := Q -> isPrimary(Q, radical Q)
isPrimary(Ideal,Ideal) := (Q,P) -> (
     if isPrime P then Q == top Q
     else false
     )
isPrimary (Module, Module) := (M, Q) -> #associatedPrimes(M/Q) == 1

--------------------------------------------------------------------
-- Associated Primes
--------------------------------------------------------------------

associatedPrimes MonomialIdeal := List => o -> (I) -> (
     inds := ass0 I;
     R := ring I;
     apply(inds, ind -> monomialIdeal apply(ind, v -> R_v)))

--------------------------------------------------------------------
-- Primary Decomposition
--------------------------------------------------------------------

primaryDecomposition Ideal := List => o -> (J) -> (
     R := ring J;
     (J',F) := flattenRing J;
     A := ring J';
     if not isPolynomialRing A then error "expected ideal in a polynomial ring or a quotient of one";
     if not isCommutative A then
       error "expected commutative polynomial ring";
     kk := coefficientRing A;
     if kk =!= QQ and not instance(kk,QuotientRing) then
       error "expected base field to be QQ or ZZ/p";
     if J === J'
     then primedecomp(J, o.Strategy)
     else (
	  G := map(R, ring J', generators(R, CoefficientRing => coefficientRing ring J'));
	  C := primedecomp(J', o.Strategy);
	  J.cache#"AssociatedPrimes" = apply(associatedPrimes J', P -> trim G P);
	  apply(C, Q -> trim G Q)
	  ))

primaryDecomposition MonomialIdeal := List => o -> (I) -> (
     R := ring I;
     aI := first exponents lcm I;
     J := dual I;
     if J == 1 then return {monomialIdeal(0_R)};
     M := first entries generators J;
     H := new MutableHashTable;
     scan(M, m -> (
	       s := first keys standardForm leadMonomial m;
	       Q := monomialIdeal apply(keys s, v -> R_v^(aI#v + 1 - s#v));
	       ind := sort keys s;
	       if not H#?ind then H#ind = Q
	       else H#ind = intersect(H#ind,Q)));
     apply(ass0 I, ind -> H#ind)
     )

-*
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
*-

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
     else error "unimplemented strategy"
     )

--------------------------------------------------------------------
----- Irreducible Decomposition
--------------------------------------------------------------------

irreducibleDecomposition = method();
irreducibleDecomposition MonomialIdeal := List => (I) -> (
     -- probably written by Greg Smith
     R := ring I;
     aI := first exponents lcm I;
     M := first entries generators dual I;
     apply(M, m -> (
	       s := first keys standardForm leadMonomial m;
	       if #s === 0 then return monomialIdeal 0_R;
	       monomialIdeal apply(keys s, v -> R_v^(aI#v + 1 - s#v))))
     )

--------------------------------------------------------------------
----- Tests section
--------------------------------------------------------------------

load "./PrimaryDecomposition/tests.m2"

--------------------------------------------------------------------
----- Documentation section
--------------------------------------------------------------------

beginDocumentation()

-- TODO: review
load "./PrimaryDecomposition/doc.m2"

--------------------------------------------------------------------
----- Development section
--------------------------------------------------------------------

end--

restart
debugLevel = 1
debug PrimaryDecomposition
