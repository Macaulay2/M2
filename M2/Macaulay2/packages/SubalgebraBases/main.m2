debug Core -- gets rid of "raw" error during installation. probably a better way...

-- Performs subduction using matrix of generators, M.
-- currently does not require the generators to be a Sagbi basis.

subduction = method(TypicalValue => RingElement)
subduction(Matrix, RingElement) := (M, f) -> (
    pres := makePresRing(ring M, M);
    result := pres#"fullSubstitution" internalSubduction(pres, f);
    result
    )
subduction(Matrix, Matrix) := (M, N) -> (
    pres := makePresRing(ring M, M);	
    ents := for i from 0 to (numcols N)-1 list(
    	pres#"fullSubstitution" internalSubduction(pres, N_(0,i))
	);
    matrix({ents})
    );

internalSubduction = method(TypicalValue => RingElement)
internalSubduction(PresRing, RingElement) := (pres, f) -> (
    tense := pres#"tensorRing";
    if ring f === tense then (
	f = (pres#"fullSubstitution")(f);
	)else if ring f =!= source pres#"inclusionAmbient" then (
	error "f must be from ambR or tensorRing.";
	);
        
    -- It is possible for ring f === ambient to be true but f is still from a different ring 
    -- than pres#"tensorRing". In this case, it shouldn't try to prevent an error by using "sub"
    -- or something. Instead, the following line will deliberately throw an error:
    -- (This is done because otherwise there is potential for a segfault.)
    throwError := f - 1_(source pres#"inclusionAmbient");   
    
    -- Use the same pres ring as much as possible.  
    -- M2 will automatically cache the gb calculation 
    -- as long as the pres ring is not reconstructed.
    J := gb (pres#"syzygyIdeal");
        
    F := pres#"substitution";
    M := monoid source pres#"inclusionAmbient";
    numblocks := rawMonoidNumberOfBlocks raw M;
    fMat := matrix({{pres#"inclusionAmbient"(f)}});    
    result := rawSubduction(numblocks, raw fMat, raw F, raw J);
    result = promote(result_(0,0), tense);    
    
    result
    );

-- The C++ implementation of rawSubduction could be improved.
-- Here is the code path that it takes:
-- (M2) subduction(Matrix) -> (M2) subduction(RingElement) -> (C++) rawSubduction(Matrix) -> (C++) subduction(RingElement)
-- If we deleted the C++ rawSubduction(Matrix) function and made rawSubduction take a RingElement, we could have:
-- (M2) subduction(Matrix) -> (M2) subduction(RingElement) -> (C++) subduction(RingElement)
internalSubduction(PresRing, Matrix) := (pres, M) -> (	
    ents := for i from 0 to (numcols M)-1 list(
    	internalSubduction(pres, M_(0,i))
	);
    matrix({ents})
    );

---------------------------------------------------------------------------------------
-- subalgebraBasis is needed for legacy purposes. It should not be changed or deleted. 
-- New code should use the function "sagbi."
---------------------------------------------------------------------------------------
subalgebraBasis = method(
    TypicalValue => Matrix, 
    Options => {
	Autosubduce => true,
    	Limit => 100,
    	PrintLevel => 0
	}
    );
subalgebraBasis(Matrix) := o -> gensMatrix -> (
    R := subring gensMatrix;
    gens sagbi(R,o)
    );
subalgebraBasis(List) := o -> L -> (
    gens sagbi(o, subring L)
    );
subalgebraBasis(Subring) := o -> subR -> (
    gens sagbi(o, subR)
    );
---------------------------------------------------------------------------------------

sagbi = method(
    TypicalValue => Subring, 
    Options => {
	Autosubduce => true,
    	Limit => 100,
    	PrintLevel => 0,
        storePending => true
    	}
    );

sagbi(Matrix) := o -> gensMatrix -> (
    sagbi(o, subring gensMatrix)
    );

sagbi(List) := o -> L -> (
    sagbi(o, subring L)
    );

sagbi(Subring) := o -> S -> (
    sagbi(o, sagbiBasis S)
    );


-- PrintLevel > 0: Print some information each loop, but don't print any polynomials.
-- PrintLevel > 1: Print new Sagbi gens.
sagbi(SAGBIBasis) := o -> S -> (
    if (S#"stoppingData"#"limit" > o.Limit) or S#"sagbiDone" then return S;
    
    compTable := new MutableHashTable from S;
    compTable#"pending" = new MutableHashTable from compTable#"pending";
    compTable#"stoppingData" = new MutableHashTable from compTable#"stoppingData";
    compTable#"stoppingData"#"limit" = max {compTable#"stoppingData"#"limit",o.Limit};
    
    if o.Autosubduce then(
	if o.PrintLevel > 0 then (
	    print("Performing initial autosubduction...");
	    );
    	compTable#"subringGenerators" = autosubduce compTable#"subringGenerators";
    );
    
    if (numcols compTable#"sagbiGenerators" == 0) or (not o.storePending) then (
    	insertPending(compTable, compTable#"subringGenerators");
        -- Remove elements of coefficient ring
        remove(compTable#"pending", 0);
        compTable#"stoppingData"#"degree" = processPending(compTable) + 1;
    	);

    local subducted;
    local newElements;
    local pres;
    local sagbiGB;
    local zeroGens;
    local syzygyPairs;
    local terminationCondition0;
    local terminationCondition1;
    local terminationCondition2;

    while compTable#"stoppingData"#"degree" <= o.Limit and not compTable#"sagbiDone" do (  	
	if o.PrintLevel > 0 then (
	    print("---------------------------------------");
	    print("-- Current degree:"|toString(compTable#"stoppingData"#"degree"));
	    print("---------------------------------------");
	    );
	
    	if o.PrintLevel > 0 then (
    	    print("-- Computing the kernel of the substitution homomorphism to the initial algebra...");
	    );
        sagbiGB = gb(compTable#"presentation"#"syzygyIdeal", DegreeLimit => compTable#"stoppingData"#"degree");
	zeroGens = submatByDegree(mingens ideal selectInSubring(1, gens sagbiGB), compTable#"stoppingData"#"degree");
	syzygyPairs = compTable#"presentation"#"substitution"(zeroGens);
	terminationCondition1 = rawStatus1 raw sagbiGB == 6;

	-- Have we previously found any syzygies of degree currDegree?
        if compTable#"pending"#?(compTable#"stoppingData"#"degree") then (
            syzygyPairs = syzygyPairs |
                compTable#"presentation"#"inclusionAmbient"(matrix{toList compTable#"pending"#(compTable#"stoppingData"#"degree")});
            remove(compTable#"pending", compTable#"stoppingData"#"degree");
            );

	if o.PrintLevel > 0 then(
    	    print("-- Performing subduction on S-polys... ");
	        print("-- Num. S-polys before subduction: " | toString(numcols syzygyPairs));
	    );

    subducted = internalSubduction(compTable#"presentation", syzygyPairs);

    if numcols subducted != 0 then (
	    newElements = compress ((compTable#"presentation"#"projectionAmbient")(subducted));
            ) else (
	    newElements = subducted;
	    );

	if o.PrintLevel > 0 then(
	    print("-- Num. S-polys after subduction: " | toString(numcols newElements));
	    );

	if o.PrintLevel > 1 then(
	    print("-- New generators:");
	    if(numcols newElements == 0) then(
		-- It has to treat this as a special case because zero matrices are special.
		    print("| 0 |");
		    )else(
		    debugPrintMat(newElements);
		    );
        );

	if numcols newElements > 0 then (
	    insertPending(compTable, newElements);
    	    processPending(compTable);
	    if not lowestDegree(compTable) == infinity then 
                 compTable#"stoppingData"#"degree" = lowestDegree(compTable)
                 else
                 compTable#"stoppingData"#"degree" = compTable#"stoppingData"#"degree" + 1;
        ) else (

        terminationCondition0 = #(compTable#"pending") == 0;
        terminationCondition2 = compTable#"stoppingData"#"degree" > max flatten (degrees compTable#"subringGenerators")_1;

        if o.PrintLevel > 0 then(
		print("-- No new generators found. ");
		print("-- Stopping conditions:");
		print("--    No higher degree candidates: "|toString(terminationCondition0));
		print("--    S-poly ideal GB completed:   "|toString(terminationCondition1));
		print("--    Degree lower bound:          "|toString(terminationCondition2));
		);

        if terminationCondition0 and terminationCondition1 and terminationCondition2 then (
            compTable#"sagbiDone" = true;
            );
	
        compTable#"stoppingData"#"degree" = compTable#"stoppingData"#"degree" + 1;
        );
    
    );
    
    if o.PrintLevel > 0 then(
    	if not compTable#"sagbiDone" then (
            print("-- Limit was reached before a finite SAGBI basis was found.");
    	    )else(
            print("-- Finite Sagbi basis was found.");
            );
    	);
    
    -- We return a new instance of subring instead of the generators themselves so that we can say whether or not a Subring instance
    -- IS a Sagbi basis, not whether or not it HAS a Sagbi basis. (The latter is unacceptable because the cache should not effect 
    -- the value of a function.)
        
    -- If subalgebraBasis is called on a Subring instance with a previously computed Sagbi basis that is not itself a Sagbi basis,
    -- a new subring instance will be constructed from its cached SagbiGens. This is OK because different instances of the same 
    -- subring will still be equal if we calculate equality based on the mathematical equality of the subalgebras they generate.
    -----------------------------------------------------------------------------------------------------
    -- subR.cache.SagbiDone: Indicates whether or not the Subring instance has a cached Sagbi basis. 
    -- subR.isSagbi        : Indicates whether or not (gens subR) itself is a Sagbi basis.
    -----------------------------------------------------------------------------------------------------
    -- The correct way to implement a function that requires a Subring instance that is a Sagbi basis is to check that 
    -- (subR.isSagbi == true). If (subR.isSagbi == false) and (subR.cache.SagbiDone == true), an error should still be thrown.
    
    sagbiBasis(storePending => o.storePending,compTable)
);


-- checks whether or not the generators form a sagbi basis wrt the given term order
verifySagbi = method()
verifySagbi Subring := S -> (
    presS := S#"presentation";
    IA := presS#"syzygyIdeal";
    GBIA := gens gb IA;
    monomialSyzygies := selectInSubring(1, GBIA);
    remainders := compress subduction(gens S, presS#"fullSubstitution" monomialSyzygies);
    HT := new MutableHashTable from S;
    HT#"isSAGBI" = (numcols remainders == 0);
    new Subring from HT
    )
verifySagbi Matrix := M -> verifySagbi subring M
verifySagbi List := L -> verifySagbi subring L
