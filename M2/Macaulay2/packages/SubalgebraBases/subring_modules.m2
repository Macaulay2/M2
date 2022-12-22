Subring ^ ZZ := Subring => (subR, n) -> (
    tense := subR#"presentation".tensorRing;
    amb := ambient subR;
    if n <= 0_ZZ then (
	error "Module rank must be positive.";
	);
    (A,B,C) := moduleToSubringIdeal(subR, id_((ambient subR)^n));
    -- In this special case, B = transpose C.
    (A, C)
    );

-- !!! 	   This is a highly experimental function.     !!!
-- If M is the output of toricSyz, this produces a Subring instance and a 1-column 
-- matrix can be used with the function autoreduce. 
moduleToSubringIdeal = method()
moduleToSubringIdeal(Subring, Matrix) := (subR, M) -> (
    
    -- It requires subR to be a Sagbi basis so that it can run extra checks to verify
    -- assumptions about the properties of the resulting subring. 
    -- TODO: consider the implications of removing these checks so that this function
    -- can work when subR isn't a Sagbi basis.
    if not subR#"isSAGBI" then(
	error "Only modules over Subring instances that are a sagbi basis are currently supported.";
	);
    
    if M%subR != 0 then(
	error "M is supposed to have entries that are elements of subR.";
	); 
    
    -- The value of these generators doesn't matter, but they should be increasing under
    -- the monomial order.
    tense := subR#"presentation".tensorRing;
    dummy := for i from 1 to numcols M list( ((vars tense)_(0,0))^i );
    dummy = matrix({dummy});
    monoRing := subring(dummy);
    gVars := genVars(monoRing);
    
    M1 := subR#"presentation"#"inclusionAmbient";
    M2 := monoRing#"presentation"#"inclusionAmbient";
    
    result2 := ((M2 M1 M)*(transpose gVars));
        
    -- This call to sagbi *should* terminate quickly if subR is a Sagbi basis.    
    coolRing := subring sagbi subring ((M2 M1 gens subR)|gVars);    
    
    -- If either of these checks fail, it could mean that the monomial order on coolRing isn't behaving.
    if not coolRing#"isSAGBI" then(
	error "Could not construct a sagbi basis of module's ring. ";
	);
    assert(result2%coolRing == 0);
    
    (coolRing, result2, gVars)
    );

-- Basically, this is the inverse of moduleToSubringIdeal.
    -- M is a 1-column matrix that is probably the result of a call to moduleToSubringIdeal
    -- gVars is a 1-row matrix containing the variables that correspond to the generators of the module.
extractEntries = method()
extractEntries(Matrix, Matrix) := (M, gVars) -> (    
    -- Sorting is done to produce consistent behavior, but the monomial order on gVars is arbitrary.
    gVars = matrix({sort first entries gVars});
    mat := for i from 0 to (numrows M)-1 list(
    	apply(first entries gVars, var -> monoCoef(var, M_(i,0)))	
	);
    matrix(mat)
    );

-- Performs autoreduction on M treated as a module over subR.
mingensSubring = method(TypicalValue => Matrix)
mingensSubring(Subring, Matrix) := (subR, M) -> (  
    (A, B, gVars)  := moduleToSubringIdeal(subR, M);
    final := autoreduce(A, transpose B);
    -- Sort the rows of the matrix for more predictable behavior.
    final = matrix transpose {sort first entries transpose final};
    final = extractEntries(final, gVars);
    subR#"presentation"#"fullSubstitution"(sub(final,subR#"presentation".tensorRing))
    );

-- For polynomial p monomial m, extract the coefficient of m in p. For example:
-- p = x*y*z + z*y^2 + 2*x^2*y^2*z^2
-- m = x*y
-- Then:
-- p = (z+2x*y*z^2)*m + z*y^2 
-- monoCoef(p, m) = z+2x*y*z^2.
-- (Notice that the coefficient of the monomial m may involve m.)
-- (Also, if m has a coefficient other than one it will be ignored.)
monoCoef = method(TypicalValue => RingElement)
monoCoef(RingElement, RingElement) := (m, p) -> (
    
    -- This does not completely guarantee that they are elements of the same ring... 
    if ring m =!= ring p then (
	error "monoCoef expected m and p to be elements of the same ring.";
	);
    supp := support(m);
    R := ring(m);
    if numcols monomials m != 1 or supp == {} then(
	error "monoCoef expects m to be a non-constant monomial.";
	);
    -- remove the coefficient of m.
    m = toMonomial(ring m, first exponents m);

    monosP := monomials p;
    coefs := for i from 0 to (numcols monosP)-1 list(
	mono := monosP_(0,i);
	if gcd(m, mono) == leadTerm m then(
	    (coefficient(mono, p))*toMonomial(R, first ((exponents mono)-(exponents m)))
	    ) else(
	    0
	    )
	);
    sum coefs   
    );

