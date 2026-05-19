entryCalculatorModified = (mu,j) -> (
    -- Calculates the (mu,j)'th entry of the A matrix
    -- mu is a composition and j ranges from 0 to n
    -- Note the we get compositions with strictly positive parts
    -- by taking compositions with non-negative parts and adding one
    u := 0;
    for mui in (reverse mu) do (
	u=u+mui+1;
	if u == j+1 then (
		if mui == 0 then
			return 2
		else 
			return 1
		);
	);
    0
)

promoteMaptoComplex = (d,k,C) -> (matrix d)**id_(C_k)

promoteFaceMapZerotoComplex = (d,C) -> matrix (
   -- This runs over the whole matrix again and so is probably not an efficient way of doing things
   -- Having the output of FaceMapZero be a hash table would be faster
   d / (i -> apply(i,j -> (
	       if j<0 then (
	           id_(C_(abs(j+1)))
    	    	    -- abs because identities were stored as negatives
		    -- +1 first because we stored identity of C_k as -k-1
	       ) 
	       else if j>0 then (
		   dd^C_(j-1)
    	    	    -- -1 because we stored the differential of C_k as k+1
		    -- I don't actually think we need to do that though?  probably could just store it as k
		   ) 
	       else 0
	    )
	)
    )
   )

zeroMatrix = (numRow,numCol) -> toList (numRow:(toList(numCol:0)));

-- AMatrix(n,k) builds all n+1 columns of the A-matrix in a single pass over
-- compositions(k+1, n-k). Each composition mu contributes non-zero entries to
-- at most (k+1) distinct columns (one per partial sum from the right), so the
-- total work is |compositions| * (k+1), not (n+1) * |compositions| * (k+1) as
-- it would be if we called ACol independently for each column.
AMatrix = memoize((n,k) -> (
    comps := compositions(k+1,n-k);
    numMu := #comps;
    numCols := n+1;
    cols := for j from 0 to numCols-1 list new MutableList from toList(numMu:0);
    for r from 0 to numMu-1 do (
	mu := comps#r;
	u := 0;
	muLen := #mu;
	for idx from 0 to muLen-1 do (
	    mui := mu_(muLen-1-idx);
	    u = u + mui + 1;
	    if u <= numCols then
		(cols#(u-1))#r = (if mui == 0 then 2 else 1);
	    );
	);
    cols/toList
    ))

ACol = (n,k,j) -> (AMatrix(n,k))#j

rowOfId = (n,l) -> splice {l:0,1,(n-1-l):0}
-- n-1 so that the result has n-many columns
    

faceMapZero = (n,len) -> (
    -- Output is topDeg-many rows and sum(len,i->binomial(n,i))-many columns
    -* This probably can be optimized
    The output is extremely sparse, so should probably be reimplemented
    as a hash table or function of some kind that specifies rules for 
    the i,j'th entry of the table
    *-
    -- If bounded => true then do this
    -- TODO implement option toggle for bounded complexes
    -- (otherwise why sum these binomial coefficients)
    maxRows := sum(len+1,i->binomial(n-1,i));
    -- The number of rows, taking into account complex length bound
    -- When len is larger than n, we'll have 2^n many rows
    offset := 0; -- keeps track of what the top zero pad should be
    verticalStrips := for k to len list (
    -- First loop over the vertical strips of columns
	bink := binomial(n,k); -- Each strip is width binomial(n,k)
    	topZeros := zeroMatrix(offset,bink);
    -- Create appropriately sized top pad of zeros
	modifiedMat := for row to bink-1 list (
    -- This modified identity matrix uses Pascal's identity
    -- First binomial(n-1,k-1) entries will be differential
    	    if row<=binomial(n-1,k-1)-1 then splice{row:0,k+1,(bink-1-row):0}
    --k+1 to keep track of which differential we'll need (k'th diff of complex)
	    else splice{row:0, -k-1,(bink-1-row):0}
    -- -k-1 to keep track of which identity map we'll need (identity map of C_k)
	    );
	botZeros := zeroMatrix(maxRows-offset-binomial(n,k),binomial(n,k));
    -- Create appropriately sized bottom pad of zeros
    	offset = offset + binomial(n-1,k-1);
    -- Update the offset
    	verticalStrip := flatten{topZeros,modifiedMat,botZeros}
    -- Concat the modified matrix with the pads
	);
    for row to maxRows-1 list (
    -- This concats the rows of the vertical strips together
    	flatten apply(verticalStrips, strip->strip#row)
	)
    )


faceMapnk = (n,k) -> (
    -- output is binomial(n-1,k)-many rows and binomial(n,k)-many columns
    if k==n then {{0}}
    else (
	Abigvector := ACol(n,k,n);
	onePos := positions(Abigvector, i -> i==1);
	onePos / (l -> rowOfId(binomial(n,k),l))
	)
    )


faceMapik = (n,k,i) -> (
    -- I think there are supposed to be binomial(n-1,k)-many rows
    -- There are binomial(n,k)-many columns
    -- I think my timing code was wack and this is actually slower than the earlier implementation
    -- Need to go back and check that
    -- From what I recall, they were both pretty close though so it's not a high priority
    Abigvector := ACol(n,k,i);
    Asmallvector := ACol(n-1,k,i-1);
    zeroPos := positions(Abigvector, i -> i==0);
    onePos := positions(Abigvector, i -> i==1);
    outputMat := new MutableList from zeroPos / (l -> rowOfId(binomial(n,k),l));
    sumPositions := positions(Asmallvector, i->i>=1);
    for l to #sumPositions-1 do (
	myIndex := sumPositions_l;
	outputMat#myIndex = outputMat#myIndex + (rowOfId(binomial(n,k),onePos#l));
    );
    toList outputMat
)

-- Direct block-matrix construction of the i=0 face map d_0 : S_n -> S_{n-1}.
-- Bypasses the sparse encoding + promote pipeline used for generic faceMapi.
-- Pascal's identity: source block of width binomial(n,k) over C_k splits into
-- binomial(n-1,k-1) "dd" columns (target block k-1) and binomial(n-1,k) "id"
-- columns (target block k).  M2's Kronecker product A ** B replaces each entry
-- of A with that entry times B, so id_(R^a) ** M is the direct sum of a copies
-- of M -- the order matters here.
faceMap0Direct = (n, C) -> (
    (lo, hi) := concentration C;
    maxK := min(hi, n);
    R := ring C;
    blocks := for kp from 0 to maxK list (
	tarRank := binomial(n-1, kp);
	for k from 0 to maxK list (
	    if k == kp + 1 and tarRank > 0 then (
		aDD := tarRank;               -- = binomial(n-1, k-1)
		bDD := binomial(n-1, k);
		ddBlock := id_(R^aDD) ** dd^C_k;
		if bDD == 0 then ddBlock
		else ddBlock | map(target ddBlock, C_k ** R^bDD, 0)
		)
	    else if k == kp then (
		aID := binomial(n-1, k-1);
		bID := binomial(n-1, k);
		if bID == 0 then map(C_k ** R^0, C_k ** R^(aID+bID), 0)
		else if aID == 0 then id_(R^bID) ** id_(C_k)
		else map(C_k ** R^bID, C_k ** R^aID, 0) | (id_(R^bID) ** id_(C_k))
		)
	    else (
		map(C_kp ** R^(binomial(n-1, kp)), C_k ** R^(binomial(n, k)), 0)
		)
	    )
	);
    matrix blocks
    )

faceMapi = (n,i,C) -> (
    -- Need to check if this can be sped up by direct summing as lists
    -- Instead of first passing to matrices
    -- and then saving the creation of the matrix option till the very end
    (lo, hi) := concentration C;
    maxK := min(hi,n-1);
    -- Are we running till maxK? or until maxK+1?
    if i==0 then (
	rawMat := faceMap0Direct(n, C);
	-- Rewrap with source/target modules consistent with the i>0 path.
	-- faceMap0Direct uses "matrix blocks" which may create different module
	-- objects; here we rebuild source/target using the same directSum structure
	-- as the module hash table in simplicialModule(Complex,...).
	maxKn := min(hi, n);
	srcParts := for k from 0 to maxKn list directSum toList(binomial(n,k):(C_k));
	tarParts := for k from 0 to maxK list directSum toList(binomial(n-1,k):(C_k));
	srcMod := if #srcParts == 1 then srcParts#0 else directSum srcParts;
	tarMod := if #tarParts == 1 then tarParts#0 else directSum tarParts;
	map(tarMod, srcMod, matrix rawMat)
	)
    else if i==n then (
	preMat := fold(directSum,for k from 0 to maxK list (
	    promoteMaptoComplex(faceMapnk(n,k),k,C)
    	    -- Notice that we only pass in k at most n-1, so the check for k==n in faceMapi isn't needed
	    ));
	preMat | map(target preMat,C_n,0)
	)
    else (
    	preMat = fold(directSum,for k from 0 to maxK list (
	     promoteMaptoComplex(faceMapik(n,k,i),k,C)
	    ));
	preMat | map(target preMat,C_n,0)
	)
    )

degenMapik = (n, k, i) -> (
    -- We'll have binomial(n,k) many columns #A(n+1,k)=binomial(n+1,k)-many rows
    
    numCols := binomial(n,k);
    zeroes := new List from (numCols) : 0;
    col := ACol(n+1,k,i);
    sig := new MutableList from 0..(#col-1);
    l := 0;
    for j to (#col-1) do(
	if (col#j == 0) then (
	    sig#j = rowOfId(numCols,l);
	    l = l+1;
	    continue;
	    ); 
	sig#j = zeroes;
	);
    new List from sig
    )

degenMapi = (n,i,C) -> (
    maxK := min(max C,n);
    preMat := fold(directSum,for k from 0 to maxK list (
	    promoteMaptoComplex(degenMapik(n,k,i),k,C)
	    )
	);
    preMat || map(C_(n+1),source preMat,0)
    )

-- Sign of the permutation that sorts L into ascending order.  O(#L^2), fine for small d.
signOfSort = L -> (
    s := 1;
    for i from 0 to #L-2 do
        for j from i+1 to #L-1 do
            if L_i > L_j then s = -s;
    s
    )

-- Fast exterior power for matrices with at most one nonzero entry per column
-- (i.e. every column is either zero or a scalar multiple of a standard basis
-- vector).  Dold-Kan face maps d_k (k > 0) and degeneracy maps s_k have this
-- shape exactly, yet M2's generic exteriorPower iterates all binomial(m,d) x
-- binomial(n,d) minors; on the (6, k) face maps with d = 3, that is ~4.4M
-- minors per call.  Walking source d-subsets directly costs binomial(n, d).
--
-- Returns null if f doesn't have the required shape, so callers can fall back.
fastExteriorPowerSparse = (d, f) -> (
    R := ring f;
    m := numrows f;
    n := numcols f;
    if d <= 0 then return exteriorPower(d, f);
    if d > n or d > m then
        return map(exteriorPower(d, target f), exteriorPower(d, source f), 0);
    ents := entries f;
    -- colInfo_j = (rowIdx, value) for the unique nonzero, or (-1, 0_R) if the
    -- column is zero.  Bail out (return null) if any column has more than one
    -- nonzero entry.
    colInfo := for j from 0 to n-1 list (
        nz := for i from 0 to m-1 list
            if ents_i_j != 0 then (i, ents_i_j) else continue;
        if #nz > 1 then return null;
        if #nz == 0 then (-1, 0_R) else nz_0
        );
    srcSubs := subsets(n, d);
    tgtSubs := subsets(m, d);
    tgtIdx := hashTable for i to #tgtSubs - 1 list tgtSubs_i => i;
    triples := for sIdx from 0 to #srcSubs - 1 list (
        cols := srcSubs_sIdx;
        infos := for c in cols list colInfo_c;
        if any(infos, ri -> ri_0 == -1) then continue;
        rows := infos / first;
        if #(unique rows) < d then continue;
        vals := infos / last;
        sg := signOfSort rows;
        valProd := fold((x, y) -> x * y, 1_R, vals);
        (tgtIdx#(sort rows), sIdx) => sg * valProd
        );
    map(exteriorPower(d, target f), exteriorPower(d, source f), triples)
    )

-- Fast Schur functor for matrices with at most one nonzero entry per column.
-- Mirrors the logic of schur(List, Matrix) from SchurFunctors:
--   schur(lambda, f) = gN * exteriorPower(mu, f) * gM
-- where mu = conjugate(lambda) and gM, gN are the Schur change-of-basis
-- matrices.  The bottleneck in the generic code is exteriorPower(mu, f),
-- which calls exteriorPower(mu_i, f) for each part and tensors them.
-- We replace those with fastExteriorPowerSparse.
--
-- Returns null if f doesn't have the required shape (fallback signal).
fastSchurSparse = (lambda, f) -> (
    R := ring f;
    m := numrows f;
    n := numcols f;
    -- Quick sparsity check: every column must have at most one nonzero.
    ents := entries f;
    for j from 0 to n-1 do (
        nzCount := 0;
        for i from 0 to m-1 do if ents_i_j != 0 then nzCount = nzCount + 1;
        if nzCount > 1 then return null;
        );
    -- Build Schur modules for source and target (these depend only on
    -- lambda and module rank, not on the matrix entries).
    SM := schurModule(lambda, source f);
    SN := schurModule(lambda, target f);
    mu := toList conjugate new Partition from lambda;
    -- Compute exteriorPower(mu, f) using fast sparse per part, then tensor.
    fastExts := for p in mu list (
        r := fastExteriorPowerSparse(p, f);
        if r === null then return null;
        r
        );
    F := fold((a, b) -> a ** b, fastExts);
    gM := SM.cache#"Schur"_1;
    gN := SN.cache#"Schur"_0;
    result := gN * F * gM;
    (source result).cache#"Schur" = SM.cache#"Schur";
    (target result).cache#"Schur" = SN.cache#"Schur";
    result
    )

	   
	   
	   
	   
	    
