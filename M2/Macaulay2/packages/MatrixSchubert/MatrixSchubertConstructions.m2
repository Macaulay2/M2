
-- Utility routines --

--------------------------------
--auxiliary function for generating a generic matrix of z variables
--INPUT: integers n,m
--OUTPUT: an n by m generic matrix with entries z_(i,j)
--NOTE: the ring automatically comes equipped with the antidiagonal term order
--TODO: allow user to input the field they want as an option
-----------------------------------

genMat = method(
    Options => {
	CoefficientRing => QQ,
	Variable => getSymbol "z"
	}
    )
genMat (ZZ,ZZ) := o -> (n,m) -> (
    k := o.CoefficientRing;
    zEntries := flatten table (n,m,(i,j) -> (i,j));
    z := o.Variable;
    degs := apply(zEntries,i-> i_1-i_0 + m); --are there better ways to make the antidiagonal weights? prob
    Q := k(monoid[z_(1,1)..z_(n,m)]);
    Mmut := mutableMatrix(Q,n,m);
    for box in zEntries do (
        Mmut_(box) = Q_(m*(box_0) + box_1);
    );
    matrix Mmut
    )


--------------------------------
--auxiliary function for getting the index of a variable in a ring
--INPUT: an indexed variable
--OUTPUT: the index of the variable
--TODO: add docs
--SUGGESTION: (from Anton) `indexOfVariable = v -> ( i:= index v; last toList R.generatorSymbols#i )`  -- need `debug Core` to use `R.generatorSymbols`
--SUGGESTION: (from Ayah) `(expression(x_1))#1`
--SUGGESTION: (from Mahrud) `last baseName x_(1,2)`
-----------------------------------
indexOfVariable = method()
indexOfVariable RingElement := Sequence => (elem) -> (
    last baseName elem
)
indexOfVariable RingElement := List => (elem) -> (
    last baseName elem
)
indexOfVariable RingElement := ZZ => (elem) -> (
    last baseName elem
)


--------------------------------
--pads an ASM with a block identity matrix to view the ASM in a larger polynomial ring
--useful for adding and intersecting ASM ideals of differing sizes
--INPUT: an ASM, number of rows/columns to add
--OUTPUT: a new ASM from the old one with the same fulton generators, but forming a larger matrix
--TODO: add docs and tests
-----------------------------------
padASM = method()
padASM(Matrix, ZZ) := Matrix => (A,n) -> (
    if not(isASM A) then error("The input must be an alternating sign matrix.");
    m := numcols A;
    zeroMatrixRight := map(ZZ^m, ZZ^n,0);
    zeroMatrixUnder := map(ZZ^n, ZZ^m,0);
    B := id_(ZZ^n);
    matrix{{A,zeroMatrixRight},{zeroMatrixUnder,B}}
    );


--------------------------------------------
--------------------------------------------
--**Constructing ASM Varieties**--
--------------------------------------------
--------------------------------------------

-------------------------------------
--checks if a given matrix is a partial ASM
--INPUT: matrix A
--OUTPUT: true if A is a partial ASM, false otherwise
-------------------------------------
isPartialASM = method()
isPartialASM Matrix := Boolean => (A) -> (
    n := numrows(A);
    m := numcols(A);
    for i from 0 to n-1 do (
        rowPartialSum := accumulate(plus, {0}|(flatten entries(A^{i})));
        if (not(isSubset(sort unique rowPartialSum, {0,1}))) then return false;
    );
    for i from 0 to m-1 do (
        colPartialSum := accumulate(plus, {0}|(flatten entries(A_{i})));
        if (not(isSubset(sort unique colPartialSum, {0,1}))) then return false;
    );
    true
)

-------------------------------------
--INPUT: partial alternating sign matrix A
--OUTPUT: corresponding alternating sign matrix
-------------------------------------
partialASMToASM = method()
partialASMToASM Matrix := Matrix => (A) -> (
    if not(isPartialASM A) then error("The input must be a partial alternating sign matrix.");
    n := numrows(A);
    m := numcols(A);
    o := 0;
    l := 0;
    for i from 0 to n-1 do (
        if sum(flatten entries(A^{i})) == 0 then (
            o = o+1;
        );
    );
    for i from 0 to m-1 do (
	if sum(flatten entries((transpose A)^{i})) == 0 then (
	        l = l+1;
	    );
	);

    M := mutableMatrix (ZZ, n, o);
    k := 0;
    for i from 0 to n-1 do (
        if sum(flatten entries(A^{i})) == 1 then(
            for j from 0 to o-1 do(
                M_(i,j) = 0;
            );
        )
        else (
            for j from 0 to o-1 do (
                if j == k then(
                    M_(i,j) = 1;
                )
                else (
                M_(i,j) = 0;
                );
            );
            k = k+1; 
        );
    );

    B := A | matrix(M);
    q := 0;
    N := mutableMatrix(ZZ, l, m+o);
    for i from 0 to m+o-1 do (
        if sum(flatten entries((transpose B)^{i})) == 1 then (
            for j from 0 to l-1 do (
                N_(j,i) = 0;
            );
        )
        else (
            for j from 0 to l-1 do (
                if j == q then (
                    N_(j,i) = 1;
                )
                else (
                    N_(j,i) = 0;
                );
            );
        q = q+1; 
        );
    );
    C := B || matrix(N)
)

----------------------------------------
--INPUT: a list w corresponding to a permutation in 1-line notation
--OUTPUT: ANTIdiagonal initial ideal of Schubert determinantal ideal for w
--TODO: modify so it doesn't compute the WHOLE rank table, only the necessary ranks
----------------------------------------
antiDiagInit = method(
    Options => {
	CoefficientRing => QQ,
	Variable => getSymbol "z"
	}
    )
antiDiagInit Matrix := o -> A -> (
    if not(isPartialASM A) then error("The input must be a partial alternating sign matrix or a permutation.");
    zMatrix := genMat(numrows A, numcols A, CoefficientRing => o.CoefficientRing, Variable=> o.Variable); --generic matrix
    rankMat := rankTable A; --rank matrix for A
    essBoxes := essentialSet A;
    if essBoxes == {} then (
    	R := ring zMatrix;
    	return monomialIdeal(0_R)
    );
    zBoxes := apply(essBoxes, i -> flatten table(i_0, i_1, (j, k) -> (j+1, k+1))); --smaller matrix indices for each essential box
    ranks := apply(essBoxes, i -> rankMat_(i_0-1, i_1-1)); --ranks for each essential box
    antiDiagGens := new MutableList;
    for box in essBoxes do (
    	pos := position(essBoxes, i -> i == box);
    	boxSubmatrix := zMatrix^{0..(box_0-1)}_{0..(box_1-1)};
    	for x in subsets(numrows boxSubmatrix, ranks_pos+1) do (
    	    for y in subsets(numcols boxSubmatrix, ranks_pos+1) do (
                indicesList := apply(pack(2, mingle(x, reverse y)), i -> toSequence i);
                antiDiagGens#(#antiDiagGens) = product(apply(indicesList, i -> boxSubmatrix_i));
	    	);
	    );
    );
    monomialIdeal(unique flatten toList antiDiagGens)
)
antiDiagInit List := o -> w -> (
    if not(isPerm w) then error("The input must be a partial alternating sign matrix or a permutation.");
    A := permToMatrix w;
    antiDiagInit(A, CoefficientRing => o.CoefficientRing, Variable=> o.Variable)
)

----------------------------------------
--Computes rank matrix of an ASM
--INPUT: an (n x m)- partial alternating sign matrix A OR a 1-line perm w
--OUTPUT: an (n x m) integer matrix of ranks of each entry
--Author: Yuyuan Luo
--TODO: add tests for this function
----------------------------------------
rankTable = method()
rankTable Matrix := Matrix => A -> (
    if not(isPartialASM A) then error("The input must be a partial alternating sign matrix or a permutation.");
    n := numrows A;
    m := numcols A;
    rankMat := mutableMatrix A;
    boxes := flatten table(n,m,(i,j)->(i,j));
    for box in boxes do (
	rankMat_box = sum flatten entries A_{0..(box_1)}^{0..(box_0)}
	);
    matrix rankMat
    )

rankTable List := Matrix => (w) -> (
    if not(isPerm w) then error("The input must be a partial alternating sign matrix or a permutation.");
    A := permToMatrix w;
    rankTable A
)

-------------
--INPUT: a list w corresponding to a permutation in 1-line notation
--    	 OR an alternating sign matrix A
--OUTPUT: a list of boxes in the Rothe diagram for A
--TODO: add documentation
-----------------------
rotheDiagram = method()
rotheDiagram Matrix := List => (A) -> (
    if not(isPartialASM A) then error("The input must be a partial alternating sign matrix or a permutation.");
    n := numrows A;
    m := numcols A;
    listEntries := flatten table(n, m, (i, j) -> (i, j));
    ones := select(listEntries, i -> A_i == 1);
    seen := new MutableList;
    for one in ones do(
        for i from one_0 to n-1 do ( --death rays to the right
            if (A_(i,one_1) == -1) then break;
            seen#(#seen) = (i, one_1);
        );
        for i from one_1 to m-1 do (
            if A_(one_0,i) == -1 then break;
            seen#(#seen) = (one_0, i);
        );
	);
    seen = set unique toList seen;
    sort apply(toList((set listEntries) - seen), i -> (i_0+1, i_1+1))
)
rotheDiagram List := List => (w) -> (
    if not(isPerm w) then error("The input must be a partial alternating sign matrix or a permutation.");
    A := permToMatrix w;
    rotheDiagram(A)
)

-----------------------
--INPUT: a list w corresponding to a permutation in 1-line notation 
    	--OR an alternating sign matrix A
--OUTPUT: A list of boxes in the Rothe diagram for A, with their corresponding rank values 
--TODO: add documentation + examples
--Could be sped up by only computing ranks of boxes in rothe diagram
-----------------------
augmentedRotheDiagram = method()
augmentedRotheDiagram List := List => w -> (
    L := rotheDiagram(w);
    R := rankTable(w);
    apply(L, (i, j) -> ((i, j), R_(i-1, j-1)))
)
augmentedRotheDiagram Matrix := List => w -> (
    L := rotheDiagram(w);
    R := rankTable(w);
    apply(L, (i, j) -> ((i, j), R_(i-1, j-1)))
)

-----------------------
--INPUT: a list w corresponding to a permutation in 1-line notation
    	--OR an alternating sign matrix A
--OUTPUT: a list of essential boxes in the Rothe diagram for A
-----------------------
essentialSet = method()
essentialSet Matrix := List => (A) -> (
    if not(isPartialASM A) then error("The input must be a partial alternating sign matrix or a permutation.");
    boxes := rotheDiagram(A);
    badBoxes := apply(boxes, i -> (positions(boxes, j -> (j == (i_0, i_1+1)))|positions(boxes, j -> (j == (i_0+1, i_1)))));
    essBoxes := positions(badBoxes, i -> i == {});
    boxes_essBoxes
)
essentialSet List := List => (w) -> (
    if not(isPerm w) then error("The input must be a partial alternating sign matrix or a permutation.");
    essentialSet permToMatrix w
)

-----------------------
--INPUT: a list w corresponding to a permutation in 1-line notation 
    	--OR an alternating sign matrix A
--OUTPUT: A list of boxes in the essential set for A, with their corresponding rank values 
--TODO: add documentation + examples
--Could be sped up by only computing ranks of boxes in rothe diagram
-----------------------
augmentedEssentialSet = method()
augmentedEssentialSet List := List => w -> (
    L := essentialSet(w);
    R := rankTable(w);
    apply(L, (i, j) -> ((i, j), R_(i-1, j-1)))
)
augmentedEssentialSet Matrix := List => A -> (
    L := essentialSet(A);
    R := rankTable(A);
    apply(L, (i, j) -> ((i, j), R_(i-1, j-1)))
)

--------------------------------------------------
--INPUT: a list w corresponding to a permutation in 1-line notation
--OUTPUT: Schubert determinantal ideal for w
--------------------------------------------
schubertDeterminantalIdeal = method(
    Options => {
	CoefficientRing => QQ,
	Variable => getSymbol "z"
    }
)
schubertDeterminantalIdeal Matrix := o -> A -> (
    if not(isPartialASM A) then error("The input must be a partial alternating sign matrix or a permutation.");
 --   A = partialASMToASM(A);
    zMatrix := genMat(numrows A, numcols A, CoefficientRing=> o.CoefficientRing, Variable => o.Variable); --generic matrix
    rankMat := rankTable A; --rank matrix for A
    essBoxes := essentialSet A;
    R := ring zMatrix;
    I := ideal(0_R);
    if essBoxes == {} then (
	I.cache.ASM = A;
	return I;
	);
    zBoxes := apply(essBoxes, i -> flatten table(i_0, i_1, (j, k) -> (j+1, k+1))); --smaller matrix indices for each essential box
    ranks := apply(essBoxes, i -> rankMat_(i_0-1, i_1-1)); --ranks for each essential box
    fultonGens := new MutableList;
    for box in essBoxes do (
    	pos := position(essBoxes, i -> i == box);
        fultonGens#(#fultonGens) = (minors(ranks_pos+1, zMatrix^{0..(box_0-1)}_{0..(box_1-1)}))_*;
    );
    I = ideal (unique flatten toList fultonGens);
    I.cache.ASM = A;
    I
)
schubertDeterminantalIdeal List := o -> w -> (
    if not(isPerm w) then error("The input must be a partial alternating sign matrix or a permutation.");    
    A := permToMatrix w;
    schubertDeterminantalIdeal(A,CoefficientRing=>o.CoefficientRing, Variable => o.Variable)
)

----------------------------------------
--INPUT: a list w corresponding to a permutation in 1-line notation
--OUTPUT: list of fulton generators for schubert determinantal ideal w
---------------------------------------
fultonGens = method(
    Options => {
	CoefficientRing => QQ,
	Variable => getSymbol "z"
	}
    )
fultonGens Matrix := o -> A -> (
    (schubertDeterminantalIdeal(A,CoefficientRing => o.CoefficientRing, Variable => o.Variable))_*
)

fultonGens List := o -> w -> (
    (schubertDeterminantalIdeal(w,CoefficientRing=> o.CoefficientRing, Variable => o.Variable))_*
)


----------------------------------------
--INPUT: a list w corresponding to a permutation in 1-line notation
--OUTPUT: diagonal initial ideal, lex wrt lex, of Schubert determinantal ideal for w
----------------------------------------
diagLexInitNW = method(
    Options => {
	CoefficientRing => QQ,
	Variable => getSymbol "z"
	}
    )
diagLexInitNW Matrix := o -> A -> (
    if not(isPartialASM A) then error("The input must be a partial alternating sign matrix or a permutation.");
    I := schubertDeterminantalIdeal(A,CoefficientRing => o.CoefficientRing, Variable => o.Variable);
    R := newRing(ring I, MonomialOrder=>Lex); --making new ring with lex diagonal term order 
    monomialIdeal leadTerm sub(I, R)
)
diagLexInitNW List := o -> w -> (
    if not(isPerm w) then error("The input must be a partial alternating sign matrix or a permutation.");
    I := schubertDeterminantalIdeal(w,CoefficientRing => o.CoefficientRing, Variable => o.Variable);
    R := newRing(ring I, MonomialOrder=>Lex); --making new ring with lex diagonal term order 
    monomialIdeal leadTerm sub(I, R)
)

----------------------------------------
--INPUT: a list w corresponding to a permutation in 1-line notation
--OUTPUT: diagonal initial ideal, lex wrt lex, of Schubert determinantal ideal for w
----------------------------------------
diagLexInitSE = method(
    Options => {
	CoefficientRing => QQ,
	Variable => getSymbol "z"
	}
    )
diagLexInitSE Matrix := o -> A -> (
    if not(isPartialASM A) then error("The input must be a partial alternating sign matrix or a permutation.");
    I := schubertDeterminantalIdeal(A,CoefficientRing => o.CoefficientRing, Variable => o.Variable);
    R := ring I;
    kk := o.CoefficientRing;
    R' := kk[reverse R_*, MonomialOrder => Lex];
    f := map(R',R, apply(R_*, i-> i=>sub(i,R')));
    monomialIdeal leadTerm(f I)
)
diagLexInitSE List := o -> w -> (
    if not(isPerm w) then error("The input must be a partial alternating sign matrix or a permutation.");
    I := schubertDeterminantalIdeal(w,CoefficientRing => o.CoefficientRing, Variable => o.Variable);
    R := ring I;
    kk := o.CoefficientRing;
    R' := kk[reverse R_*, MonomialOrder => Lex];
    f := map(R',R, apply(R_*, i-> i=>sub(i,R')));
    monomialIdeal leadTerm(f I)
)

----------------------------------------
--INPUT: a list w corresponding to a permutation in 1-line notation
--OUTPUT: diagonal initial ideal, lex wrt revlex, of Schubert determinantal ideal for w
----------------------------------------
diagRevLexInit = method(
    Options => {
	CoefficientRing => QQ,
	Variable => getSymbol "z"
	}
    )
diagRevLexInit Matrix := o -> A -> (
    if not(isPartialASM A) then error("The input must be a partial alternating sign matrix or a permutation.");
    I := schubertDeterminantalIdeal(A, CoefficientRing => o.CoefficientRing, Variable => o.Variable);
    k := numrows A;
    R := ring I;
    oldvars := R_*;
    groupedvars := pack(oldvars, k);
    newvars := apply(groupedvars, i -> reverse i);
    S := QQ[flatten newvars]; --making new ring with revlex diagonal term order 
    monomialIdeal leadTerm sub(I, S)
)
diagRevLexInit List := o -> w -> (
    if not(isPerm w) then error("The input must be a partial alternating sign matrix or a permutation.");
    I := schubertDeterminantalIdeal(w,CoefficientRing => o.CoefficientRing, Variable => o.Variable);
    k := #w;
    R := ring I;
    oldvars := R_*;
    groupedvars := pack(oldvars, k);
    newvars := apply(groupedvars, i -> reverse i);
    S := QQ[flatten newvars]; --making new ring with revlex diagonal term order 
    monomialIdeal leadTerm sub(I,S)
)

-------------------------------------------
--INPUT: a list w corresponding to a permutation in 1-line notation
--OUTPUT: subword complex associated to w (i.e. SR-ideal of antiDiagInit)
--TODO: extend to more general subword complexes from Coxeter groups, not just these?
-------------------------------------------
subwordComplex = method()
subwordComplex List := SimplicialComplex => w -> (
    if not(isPerm w) then error("The input must be a permutation.");
    simplicialComplex antiDiagInit w
);

------------------------------------------
--INPUT: a nonempty list of equidimensional ASMs, presented as matrices
--OUTPUT: the minimal rank table, presented as a matrix
--TODO: tests, documentation
------------------------------------------
entrywiseMinRankTable = method()
entrywiseMinRankTable List := Matrix => L -> (
    if (#L == 0) then error("The input must be a nonempty list.");
    n := min(L/numrows);
    m := min(L/numcols);
    a := max(n,m);
    minimalRankMtx := mutableMatrix(ZZ, n, m);

    -- initialize the minimalRankMtx to something with big entries everywhere
    for i from 0 to n-1 do (
        for j from 0 to m-1 do (
            minimalRankMtx_(i, j) = a + 1;
        );
    );

    -- comb through the list to get the minimal entries
    for M in L do (
	T := rankTable M;
      	if (numrows M != n) then error ("The input must be a list of alternating sign matrices of the same size.");
      	if (numcols M != m) then error ("The input must be a list of alternating sign matrices of the same size.");	
      	if not(isPartialASM(M)) then error("The input must be a list containing partial alternating sign matrices.");

        for i from 0 to n-1 do (
            for j from 0 to m-1 do (
                minimalRankMtx_(i,j) = min{minimalRankMtx_(i,j), T_(i,j)};
            );
        );
    );
    matrix minimalRankMtx
)

------------------------------------------
--INPUT: a nonempty list of equidimensional ASMs, presented as matrices
--OUTPUT: the maximal rank table, presented as a matrix
--TODO: tests, documentation
------------------------------------------
entrywiseMaxRankTable = method()
entrywiseMaxRankTable List := Matrix => L -> (
    if (#L == 0) then error("The input must be a nonempty list.");
    n := max(L/numrows);
    m := max(L/numcols);
    maximalRankMtx := mutableMatrix(ZZ, n, m);

    -- comb through the list to get the maximal entries
    for M in L do (
	T := rankTable M;
        if (numrows M != n) then error ("The input must be a list of alternating sign matrices of the same size.");
	if (numcols M != m) then error ("The input must be a list of alternating sign matrices of the same size.");
        if not(isPartialASM(M)) then error("The input must be a list containing partial alternating sign matrices.");

        for i from 0 to (numrows M)-1 do (
            for j from 0 to (numcols M)-1 do (
                maximalRankMtx_(i,j) = max {maximalRankMtx_(i,j), T_(i,j)};
            );
        );
    );
    matrix maximalRankMtx
)

-------------------------------------------
--INPUT: an ASM ideal
--OUTPUT: the primary decomposition of the ASM ideal
--TODO: docs and tests
--TODO: input validation/type checking
-------------------------------------------
monomialRank = method()
monomialRank (RingElement, ZZ) := ZZ => (mon, maxIdx) -> (
    monIdx := indexOfVariable mon;
    (monIdx_0 + 1)*maxIdx - monIdx_1
)

-------------------------------------------
--INPUT: an ASM ideal
--OUTPUT: the primary decomposition of the ASM ideal
-------------------------------------------

schubertDecompose = method()

schubertDecompose Ideal := List => I -> (
    if I == 0 then (
        return {toList (1..floor sqrt numgens ring I)};
    );
    maxIdx := 0;
    if I.cache.?ASM then (
	M := I.cache.ASM;
        entrySum := sum flatten entries M;
	maxIdx = 2*(numcols M) - entrySum;
	permMaxIdx := numcols M;
	);
    if not(I.cache.?ASM) then (
	maxIdx = max((ring I)_* / indexOfVariable  / toList / sum);
	permMaxIdx = last indexOfVariable last ((ring I)_*);
	);
    primeDecomp := decompose ideal leadTerm I;
    -- varWeights := (monoid ring I).Options.MonomialOrder#1#1;
    cycleDecomp := new MutableList;
    for primeComp in primeDecomp do {
        mons := sort(primeComp_*, mon -> monomialRank(mon, maxIdx));
        perms := apply(mons / indexOfVariable, perm -> toAntiDiagTrans(perm, maxIdx));
        fullPerm := fold(composePerms, perms);
	if sort take(fullPerm,permMaxIdx) == toList(1..permMaxIdx) then trimmedPerm := take(fullPerm,permMaxIdx) else (
	    trimmedPermIdx := select(#fullPerm, i -> fullPerm_{i..#fullPerm-1} != toList(i+1..#fullPerm)); --THIS IS WRONG
	    trimmedPerm = fullPerm_trimmedPermIdx;
	    );
	cycleDecomp#(#cycleDecomp) = trimmedPerm;
    };
    unique toList cycleDecomp
)

schubertDecompose Matrix := List => A -> (
    if not(isPartialASM A) then error("The input must be a partial alternating sign matrix.");
    A' := partialASMToASM A;
    I := schubertDeterminantalIdeal A';
    schubertDecompose I
    )

-------------------------------------------
--INPUT: a partial ASM A
--OUTPUT: the smallest permutations bigger than A in Bruhat order
--TODO: docs and tests
--TODO: input validation/type checking
--NOTE: This assumes that schubertDecompose is allowed to take in something other than an ASM ideal.  Adjust if schubDecompose is changed.
-------------------------------------------
permSetOfASM = method()
permSetOfASM Matrix := List => A -> (
    if not(isPartialASM(A)) then error("The input must be a partial alternating sign matrix.");
    I := antiDiagInit A;
    schubertDecompose I
    )

-------------------------------------------
--INPUT: an ideal
--OUTPUT: whether the ideal is an intersection of Schubert determinantal ideals
--TODO: docs and tests
--TODO: input validation/type checking
-------------------------------------------
isIntersectionOfSchubertDeterminantalIdeals = method()
isIntersectionOfSchubertDeterminantalIdeals Ideal := Boolean => I -> (
    isIntersection := true;
    if (I == radical(I)) then {
        schubDecomp := apply(schubertDecompose I, i-> schubertDeterminantalIdeal(i, CoefficientRing => coefficientRing(ring I)));
	Q := ring schubDecomp_0;
        isIntersection = sub(I,Q) == intersect apply(schubDecomp, J -> sub(J,Q));
    }
    else {
        isIntersection = false;
    };
    isIntersection
)

-------------------------------------------
--INPUT: an ideal
--OUTPUT: whether the ideal is an ASM ideal
--TODO: docs and tests
--TODO: input validation/type checking
-------------------------------------------
isASMIdeal = method()
isASMIdeal Ideal := Boolean => (I) -> (
    ASMcheck := true;
    schubDecomp := schubertDecompose I;
    primeComps := schubDecomp / schubertDeterminantalIdeal;
    Q := ring primeComps_0;
    intersectCheck := intersect(apply(primeComps, J-> sub(J,Q)));
    if (ASMcheck = (intersectCheck == sub(I,Q))) then {
        permMatrices := (schubDecomp / permToMatrix);
        rkTable := entrywiseMaxRankTable permMatrices;
        A := rankTableToASM matrix rkTable;
        ASMIdeal := schubertDeterminantalIdeal matrix A;
--	varHash := hashTable(apply((ring ASMIdeal)_*, i-> (last baseName i)=> i));
--	phi := map(ring ASMIdeal, ring I, apply((ring I)_*, i-> varHash#(last baseName i)));
        ASMcheck = (ASMIdeal == sub(I, ring ASMIdeal));
        if ASMcheck then I.cache.ASM = A;
    }
    else {
        ASMcheck = false;
    };
    ASMcheck
)


-------------------------------------------
--INPUT: a partial alternating sign matrix
--OUTPUT: whether the matrix is an ASM matrix
--TODO: docs and tests
--TODO: input validation/type checking
-------------------------------------------
isASM = method()
isASM Matrix := Boolean => (M) -> (
    if not(isPartialASM M) then error("The input must be a partial alternating sign matrix or a permutation.");
    n := numrows(M);
    m := numcols(M);
    if (n != m) then return false;
    for i from 0 to n-1 do (
	if ((sum entries(M_{i}) != {1}) or (sum entries((transpose M)_{i}) != {1})) then return false;
    );
    true
)

-------------------------------------------
--INPUT: a list of permutations in 1 line notation
--OUTPUT: whether the union of their matrix schubert varieties is an ASM variety
--TODO: docs and tests
--TODO: input validation/type checking
-------------------------------------------
isASMUnion = method()
isASMUnion List := Boolean => (L) -> (
    if not all(L, isPerm) then error("The input be a list of permutations in 1 line notation");
    rkTable := entrywiseMaxRankTable (L / permToMatrix);
    if not isMinRankTable rkTable then return false; -- might be redundant, is the entrywise max rank table of a list of *permutations* always a min rank table?
    A := rankTableToASM rkTable;
    isSubset(set permSetOfASM A, set L)
)

-------------------------------------------
--INPUT: an ideal
--OUTPUT: the ASM of an ideal
--TODO: docs and tests
--TODO: input validation/type checking
-------------------------------------------
getASM = method()
getASM Ideal := Matrix => (I) -> (
    if I.cache.?ASM then I.cache.ASM else if isASMIdeal I then I.cache.ASM else error("given ideal is not an ASM ideal.")
)

------------------------------------------
--INPUT: a square matrix M
--OUTPUT: whether M is a valid rank table.
--TODO: documentation, tests
------------------------------------------
isMinRankTable = method()
isMinRankTable Matrix := Boolean => (A) -> (
    a := numrows A;
    b := numcols A;
--    if not(a == b) then return false;
    for i from 0 to a-1 do (    
        for j from 0 to b-1 do (
            if (i == 0 and j == 0 and not(A_(i,j) == 0 or A_(i,j) == 1)) then return false
            else if (i == 0 and j != 0 and (not(A_(i,j)-A_(i,j-1) == 0 or A_(i,j)-A_(i,j-1) == 1) or not(A_(i,j) == 0 or A_(i,j) == 1))) then return false
            else if (i != 0 and j == 0 and (not(A_(i,j)-A_(i-1,j) == 0 or A_(i,j)-A_(i-1,j) == 1) or not(A_(i,j) == 0 or A_(i,j) == 1))) then return false
            else if (i != 0 and j != 0 and (not(A_(i,j)-A_(i,j-1) == 0 or A_(i,j)-A_(i,j-1) == 1) or not(A_(i,j)-A_(i-1,j) == 0 or A_(i,j)-A_(i-1,j) == 1))) then return false;
        );
    );
    true
)

------------------------------------------
--INPUT: a rank table, presented as a matrix
--OUTPUT: an ASM corresponding to the rank table, presented as a matrix
--TODO: documentation and tests
------------------------------------------
rankTableToASM = method()
rankTableToASM Matrix := Matrix => (A) -> (
    if not(isMinRankTable(A)) then error("The inputted matrix is not a valid minimal rank table.");
    n := numrows A;
    m := numcols A;
    ASMret := mutableMatrix(ZZ, n, m);
    for i from 0 to n-1 do (
        for j from 0 to m-1 do (
            if (i == 0 and j == 0) then (
                if (A_(0,0) == 1) then (ASMret_(0,0) = 1;);
            )
            else if (i == 0) then (
                if (A_(i,j) == 1 and A_(i,j-1)==0) then (ASMret_(i,j) = 1;);
            )
            else if (j == 0) then (
                if (A_(i,j) == 1 and A_(i-1,j)==0) then (ASMret_(i,j) = 1;);
            )
            else (
                if (A_(i,j) - A_(i,j-1) == 1 and A_(i,j) - A_(i-1,j) == 1 and A_(i-1,j) == A_(i-1,j-1)) then (ASMret_(i,j) = 1;)
                else if (A_(i,j) == A_(i,j-1) and A_(i,j) == A_(i-1,j) and A_(i,j) > A_(i-1,j-1)) then (ASMret_(i,j) = -1;);
            );
        );
    );
    matrix ASMret
)

--------------------------------------------
-- INPUT: an integer matrix M where the entries are at least 0
-- OUTPUT: the minimal rank table associated to M representing an ASM 
-- TODO: tests and documentation
--------------------------------------------
rankTableFromMatrix = method()
rankTableFromMatrix Matrix := Matrix => A -> (
    if not ((ring A) === ZZ) then error ("Must be an integer matrix.");
    n := numrows A;
    m := numcols A;
    rankTable := mutableMatrix(ZZ,n,m);
--    if not(#(AList#0) == n) then error("Must be a square matrix.");

    for i from 0 to n-1 do (
        for j from 0 to m-1 do(
            if (A_(i,j) < 0) then error("Must be a matrix with nonnegative entries.");
            if (i == 0 and j == 0) then (
                rankTable_(n-1, m-1) = min(n,m, A_(n-1,m-1));
            )
            else if (i == 0) then (
                rankTable_(n-1-i, m-1-j) = min(n-i, m-j, A_(n-1-i, m-1-j), rankTable_(n-1-i,m-j));
            )
            else if (j == 0) then (
                rankTable_(n-1-i, m-1-j) = min(n-i, m-j, A_(n-1-i,m-1-j), rankTable_(n-i,m-j-1));
            )
            else (
                rankTable_(n-1-i, m-1-j) = min(n-i, m-j, A_(n-1-i,m-1-j), rankTable_(n-1-i, m-j), rankTable_(n-i,m-j-1));
            );
        );
    );

    for i from 0 to n-1 do (
        for j from 0 to m-1 do(
            if (i == 0 and j == 0) then (
                rankTable_(i, j) = min(1, rankTable_(i, j));
            )
            else if (i == 0) then (
                rankTable_(i, j) = min(rankTable_(i, j), rankTable_(i, j-1)+1);
            )
            else if (j == 0) then (
                rankTable_(i, j) = min(rankTable_(i, j), rankTable_(i-1, j)+1);
            )
            else (
                rankTable_(i, j) = min(rankTable_(i, j), rankTable_(i, j-1)+1, rankTable_(i-1, j)+1);
            );
        );
    );
    matrix rankTable
)

--------------------------------------------
-- INPUT: a list of permutations or ASMs
-- OUTPUT: the intersection of the ideals 
-- TODO: tests and documentation
--------------------------------------------
schubertIntersect = method()
schubertIntersect List := Ideal => (L) -> (
    if (#L == 0) then error("Please enter a nonempty list.");
    ll := L / schubertDeterminantalIdeal;
    numVars := apply(ll, i-> #((ring i)_*));
    Q := ring ll_(position(numVars, i-> i == max numVars));
    intersect apply(ll, J -> sub(J, Q))
);

--------------------------------------------
-- INPUT: a list of permutations or ASMs
-- OUTPUT: the sum of the ideals 
-- TODO: tests and documentation
--------------------------------------------
schubertAdd = method()
schubertAdd List := Ideal => (L) -> (
    if (#L == 0) then error("Please enter a nonempty list.");
    listASMs := L / (i -> if instance(i, Matrix) then (partialASMToASM i) else if instance(i_0, List) then matrix i else (permToMatrix i));
    n := max(apply(listASMs, i-> numrows i));
    paddedASMs := apply(listASMs, A -> padASM(A, n - (numrows A)));
    rankM := entrywiseMinRankTable(paddedASMs);
    sumI := schubertDeterminantalIdeal rankTableToASM(rankM);
    sumI.cache.rankTable = rankM;
    sumI
);

--------------------------------------------
-- INPUT: a partial alternating sign matrix
-- OUTPUT: the permutation realizing the ASM if the ASM is a permutation matrix,
   --otherwise returns the empty permutation
-- TODO: tests and documentation
--------------------------------------------
-*
getPermFromASM = method()
getPermFromASM Matrix := List => (A) -> (
    if not(isPartialASM A) then error("The input must be a partial alternating sign matrix or a permutation.");
    if not(numrows(A) == numcols(A)) then return {};
    w := {};
    n := numrows(A);
    m := numcols(A);
    --Make sure that A is a permutation matrix
    for i from 0 to n-1 do (
        for j from 0 to m-1 do (
            if (A_(i,j) == 1) then (
                w = w | {j+1};
            ) else if (A_(i,j) != 0) then (
                return {};
            );
        );
    );
    if not(length(w) == numrows(A)) then return {};
    if (isPerm(w) == true) then return w
    else return {};
);
*-
