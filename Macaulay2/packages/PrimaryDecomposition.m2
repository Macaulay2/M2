-- -*- coding: utf-8 -*-
newPackage(
     "PrimaryDecomposition",
     AuxiliaryFiles => true,
     Headline => "functions for primary decomposition (pre-loaded)"
     )
export {
     isPrimary,
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
     else error "unimplemented strategy"
     )

primaryDecomposition Ideal := List => o -> (J) -> (
     R := ring J;
     (J',F) := flattenRing J;
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

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/packages PACKAGES=PrimaryDecomposition pre-install"
-- End:
