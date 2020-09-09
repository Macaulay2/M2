-- -*- coding: utf-8 -*-
newPackage(
     "PrimaryDecomposition",
     Version => "1.0",
     Date => "July 1, 2008",
     AuxiliaryFiles => true,
     Authors => {{Name => "Michael E. Stillman", Email => "mike@math.cornell.edu"}},
     Keywords => {"Commutative Algebra"},
     Headline => "functions for primary decomposition"
     )

export {
     "primaryDecomposition",
     "irreducibleDecomposition",
     "isPrimary",
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
     "primaryComponent"
--     "quotMin",
--     "radicalContainment"
     }

-- private symbols used as keys:
protect H, protect U, protect W

--     EHVprimaryDecomposition,			    -- cryptic
--     HprimaryDecomposition,
--     Hybrid,
--     primdecComputation,
--     minSatPPD,
--     sortByDegree

primaryDecomposition = method( TypicalValue => List, Options => { Strategy => null } )


load "./PrimaryDecomposition/GTZ.m2"
load "./PrimaryDecomposition/Shimoyama-Yokoyama.m2"
load "./PrimaryDecomposition/Eisenbud-Huneke-Vasconcelos.m2"
load "./PrimaryDecomposition/radical.m2"

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
     else error "unimplemented strategy"
     )

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

isPrimary = method()
isPrimary(Ideal) := Q -> isPrimary(Q, radical Q)
isPrimary(Ideal,Ideal) := (Q,P) -> (
     if isPrime P then Q == top Q
     else false
     )

minimalPrimes MonomialIdeal := decompose MonomialIdeal := (cacheValue symbol minimalPrimes) (
     (I) -> (
	  minI := dual radical I;
          if minI == 1 then {monomialIdeal(0_(ring I))}
          else
	      apply(flatten entries generators minI, monomialIdeal @@ support)))

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

associatedPrimes MonomialIdeal := List => o -> (I) -> (
     inds := ass0 I;
     R := ring I;
     apply(inds, ind -> monomialIdeal apply(ind, v -> R_v)))

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

beginDocumentation()

document {
     Key => PrimaryDecomposition,
     Headline => "functions for primary decomposition",
     "This package provides computations with components
     of ideals, including minimal and associated primes, radicals, and
     primary decompositions of ideals.",
     Subnodes => {
	  TO (associatedPrimes, Ideal),
	  TO (localize,Ideal,Ideal),
	  TO [localize,Strategy],
	  TO (primaryComponent, Ideal, Ideal),
	  TO [primaryComponent,Strategy],
	  TO [primaryComponent,Increment],
	  TO (primaryDecomposition, Ideal),
	  TO [primaryDecomposition,Strategy]
	  },
     SeeAlso => { (primaryDecomposition, Ideal) }
     }

load "./PrimaryDecomposition/doc.m2"

TEST ///
     testResult = method()
     testResult(Ideal,List) := (I,L) -> (
	  assert(#L > 0);
	  scan(L, J -> assert(isIdeal J and ring J === ring I));
	  assert(I == intersect L);
	  if #L > 1 then (
	       scan(#L, i -> (
		    	 L2 := L_(select(toList(0 .. (#L-1)), j -> j != i));
		    	 assert(I != intersect L2);
		    	 )
	       	    );
	       );
	  L3 := associatedPrimes I;
	  assert(#L == #L3);
	  scan(#L, i -> (
		    J := L_i;
		    P := radical J;
		    assert(P == L3_i);
		    if isPrimary(J,P) then (
			 
			 )
		    else (
			 print(ring I);
			 print I;
			 print L;
			 print J;
			 assert false;
			 );
		    )
	       );
	  )
          
     w,x,y,z     

     scan({QQ, ZZ/3, ZZ/2, ZZ/101, ZZ/32003}, k -> (
	       Q := k[w,x,y,z];
	       scan({ideal(x*y,y^2), ideal(x^4*y^5), ideal(w*x, y*z, w*y+x*z),
			 intersect((ideal(w,x,y-1))^2, ideal(y,z,w-1))}, I -> (
			 sl := {EisenbudHunekeVasconcelos, ShimoyamaYokoyama,
	       			   new Hybrid from (1,2), new Hybrid from (2,2)};
			 if isMonomialIdeal I then sl = {Monomial} | sl;
			 scan(sl, s -> (
	       		 	   testResult(I, primaryDecomposition(I, Strategy => s))
			 	   )
			      )	    
	       	    	 )     
	       	    );
	       scan({new Hybrid from (1,1), new Hybrid from (2,1)}, s -> (
			 testResult(ideal(x^4*y^5), primaryDecomposition(ideal(x^4*y^5), Strategy => s))
			 )
		    )	    
	       )
	  )
///

TEST ///
  -- trivial cases:
  R = QQ[x,y,z]
  I = monomialIdeal(1_R)
  assert(primaryDecomposition I == {})
  assert not isPrimary I
  assert not isPrime I
  assert(minimalPrimes I == {})

  I = ideal(1_R)
  assert(primaryDecomposition I == {})
  assert not isPrimary I
  assert not isPrime I
  assert(minimalPrimes I == {})

  I = ideal(0_R)
  assert(primaryDecomposition I == {trim ideal(0_R)})
  assert isPrimary I
  assert isPrime I
  assert(minimalPrimes I == {ideal(0_R)})

  I = trim ideal(0_R)
  assert(primaryDecomposition I == {trim ideal(0_R)})
  assert isPrimary I
  assert isPrime I
  assert(minimalPrimes I == {ideal(0_R)})

  I = monomialIdeal(0_R)
  assert(primaryDecomposition I == {monomialIdeal(0_R)})
  assert isPrimary I
  assert isPrime I
  assert(minimalPrimes I == {ideal(0_R)})
  assert(all(minimalPrimes I, f -> class f === MonomialIdeal))
///
-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=PrimaryDecomposition pre-install"
-- End:
