----------------------------------------------------------------
--************************************************************--
--Functions for computing compatibly split ideals             --
--************************************************************--
----------------------------------------------------------------

-----------------------------------------------------------------------


--- Start of MK ---------------------------------------------------------------------------------------------------

-- FIND IDEALS COMPATIBLE WITH A GIVEN NEAR-SPLITTING
-- This is an implementation of the algorithm described in
-- Moty Katzman and Karl Schwede's paper 
-- "An algorithm for computing compatibly Frobenius split subvarieties"
-- J. Symbolic Comput. 47 (2012), no. 8, 996\961008. 

----------------------------------------------------------------------------------------


--- Input:
---   	an element u of the polynomial ring R OVER A PRIME FIELD.
--- Output:
---	A list of all prime ideals P such that
---	(a) u P \subseteq P^{[p]}, and
---	(b) the action of uT on the annihilator of P on the injective hull of the residue field of R 
---	is not the zero Frobenius map.

compatibleIdeals = method( Options => { FrobeniusRootStrategy => Substitution } )

compatibleIdeals RingElement := o -> u ->
(
    if not isPolynomialOverPrimeField( u ) then 
        error "compatibleIdeals: expected an element of a polynomial ring over a prime field ZZ/p";
    L := { }; 
    R := ring u; 
    P := ideal 0_R;
    J:=frobeniusRoot( 1, ideal u );
    t := 1_R % ( gens J );
    if t != 0_R then print "compatibleIdeals: *** WARNING *** Frobenius action has nilpotent elements";
    compatibleIdealsInnards ( u, L, P )
)

compatibleIdealsInnards = method(Options => {FrobeniusRootStrategy => Substitution})

compatibleIdealsInnards(RingElement, List, Ideal) := o-> ( u, L, P ) ->
(
    local f;
    P1 := frobenius P;
--    C1 := ideal( ( singularLocus( P ) ).relations );
    C1 := ideal testElement((ring P)/P, AssumeDomain => true);
    ---tau=ideal mingens star(C1,u,1) ; ---OLD VERSION
    tau := ideal mingens ascendIdeal( 1, u, C1, o );
    Plist := minimalPrimes tau;
    apply( Plist, Q ->
        (
    	    f = any( L, T -> T == Q );
            if not f then
	    (
	        L = append( L, Q );
	        L = unique( L | compatibleIdealsInnards( u, L, Q, o ) );
	    );
        )
    );
---
    C2 := ( P1 + ideal( u ) ) : ( P1 : P );
---	JB:=C1*C2; ---MK
---print(mingens P, mingens JB);
---tau=ideal mingens star(C2,u,1) ;  --- OLD VERSION
    tau = ideal mingens ascendIdeal( 1, u, C2, o );
    Plist = minimalPrimes tau;
    apply( Plist, Q ->
	(
	    f = any( L, T -> T == Q );
	    if not f then
	    (
		L = append( L, Q );
		L = unique( L | compatibleIdealsInnards( u, L, Q, o ) );
	    );
	)
    );
    L
)
