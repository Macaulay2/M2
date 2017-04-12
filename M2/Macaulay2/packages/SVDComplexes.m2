newPackage(
        "SVDComplexes",
        Version => "0.1", 
        Date => "",
        Authors => {{Name => "Frank Schreyer", 
                  Email => "", 
                  HomePage => ""},
              {Name => "Mike Stillman", 
                  Email => "", 
                  HomePage => ""}},
        Headline => "SVD of a complex, includes nonminimal resolutions over the reals",
        DebuggingMode => true
        )

export {
    "constantStrand", -- documented, tests
    "constantStrands", -- documented, tests
    "laplacians",
    "getNonminimalRes",
    "degreeZeroMatrix",
    "minimizeBetti",
    "SVDComplex",
    "SVDHomology",
    "pseudoInverse",
    "SVDBetti",
    "Projection",
    "Laplacian",
---    "Threshold",
    "numericRank",
    "commonEntries",
    "uniquify",
    "checkSVDComplex"
}

--------------------------------------------------
-- Some basic functions that should not be here --
-- Move these to the core? -----------------------
--------------------------------------------------
debug Core
chainComplex(HashTable) := (maps) -> (
    -- maps should be a HashTable with keys integers.  values are maps at that spot.
    rgs := (values maps)/ring//unique;
    if #rgs != 1 then error "expected matrices over the same ring";
    R := rgs#0;
    C := new ChainComplex;
    C.ring = R;
    for i in keys maps do (
        f := maps#i;
        F := source f;
        G := target f;
        if C#?i then (if C#i =!= F then error("different modules at index "|i))
        else C#i = F;
        if C#?(i-1) then (if C#(i-1) =!= G then error("different modules at index "|i-1))
        else C#(i-1) = G;
        );
    C.dd.cache = new CacheTable;
    lo := min keys maps - 1;
    hi := max keys maps;
    for i from lo+1 to hi do C.dd#i = if maps#?i then maps#i else map(C_i, C_(i-1), 0);
    C
    )

newChainComplexMap = method()
newChainComplexMap(ChainComplex, ChainComplex, HashTable) := (tar,src,maps) -> (
     f := new ChainComplexMap;
     f.cache = new CacheTable;
     f.source = src;
     f.target = tar;
     f.degree = 0;
     goodspots := select(spots src, i -> src_i != 0);
     scan(goodspots, i -> f#i = if maps#?i then maps#i else map(tar_i, src_i, 0));
     f
    )



-------------------------------------------------
-- Code for random complexes over the integers --
-------------------------------------------------
randomUpper = method(Options=>{Height=>10})
randomUpper ZZ := opts -> n -> (
    if n == 0 then return map(ZZ^0, ZZ^0, {});
    matrix(ZZ, for i from 0 to n-1 list for j from 0 to n-1 list (
	    if i > j then 0
	    else if i == j then 1 
            else random(ZZ,opts) - floor(opts.Height/2)
            ))
    )
randomSL = method(Options=>{Height=>10})
randomSL(ZZ) := opts -> n -> randomUpper(n,opts) * transpose randomUpper(n,opts)


-----------------------------------------------
-- Code for nonminimal resolutions over QQ ----
-----------------------------------------------
isMadeFromFastNonminimal = (C) -> C.?Resolution and C.Resolution.?RawComputation
fastNonminimalComputation = (C) -> if C.?Resolution and C.Resolution.?RawComputation then C.Resolution.RawComputation else null

constantStrand = method()
constantStrand(ChainComplex, Ring, ZZ) := (C, kk, deg) -> (
    -- assumption: we are resolving an ideal, or at least all gens occur in degree >= 0.
    comp := fastNonminimalComputation C;
    if comp === null then error "currently expect chain complex to have been constructed with res(...,FastNonminimal=>true)";
    len := length C;
    reg := regularity C;
    chainComplex for lev from 1 to len list (
        matrix map(kk, rawResolutionGetMutableMatrix2B(comp, raw kk, deg,lev))
        )
    )    

constantStrand(ChainComplex, ZZ) := (C, deg) -> (
    kk := coefficientRing ring C;
    if kk === QQ then error "coefficient ring is QQ: need to provide a ring: RR_53, RR_1000, ZZ/1073741891, or ZZ/1073741909, or ZZ";
    comp := fastNonminimalComputation C;
    if comp === null then error "currently expect chain complex to have been constructed with res(...,FastNonminimal=>true)";
    -- assumption: we are resolving an ideal, or at least all gens occur in degree >= 0.
    len := length C;
    reg := regularity C;
    chainComplex for lev from 1 to len list (
        matrix map(kk, rawResolutionGetMutableMatrix2B(comp, raw kk, deg,lev))
        )
    )    

constantStrands = method()
constantStrands(ChainComplex, Ring) := (C, kk) -> (
    -- base ring of C should be QQ
--    if coefficientRing ring C =!= QQ then error "ring of the complex must be a polynomial ring over QQ";
    -- assumption: we are resolving an ideal, or at least all gens occur in degree >= 0.
    len := length C;
    reg := regularity C;
    hashTable for deg from 0 to len+reg list (
        D := constantStrand(C,kk,deg);
        if D == 0 then continue else deg => D
        )
    )
constantStrands ChainComplex := (C) -> constantStrands(C, coefficientRing ring C)

getNonminimalRes = method()
getNonminimalRes(ChainComplex, Ring) := (C, R) -> (
    -- if C was created using FastNonminimal=>true, then returns the nonmimal complex.
    -- if ring C is not QQ, this should be exactly C (with C.dd set).
    -- if ring C is QQ, then R must be either RR_53 (monoid ring C), or (ZZ/p)(monoid ring C), where p is the prime used to
    --  construct the resolution (later, there might be several such primes, and also we can
    --  query and get them.  But not yet.)
    rawC := C.Resolution.RawComputation;
    result := new MutableList;
    for i from 0 to length C - 1 do (
      result#i = matrix map(R, rawResolutionGetMutableMatrixB(rawC, raw R, i+1));
      << " i=" << i << " matrix = " << netList result#i << endl;
      if i > 0 then result#i = map(source result#(i-1),,result#i);
      );
    chainComplex toList result
    )

TEST ///
-- TODO for constantStrand, constantStrands:
--  a. make it work for complexes constructed in different manners, not just for FastNonminimal
--  b. allow a single multi-degree
  -- constantStrand, constantStrands
  -- these are from nonminimal free resolutions over QQ
restart
  needsPackage "SVDComplexes"
  
  R = QQ[a..e]
  I = ideal(a^3, b^3, c^3, d^3, e^3, (a+b+c+d+e)^3)
  C = res(I, FastNonminimal=>true)
  betti C
  constantStrand(C, RR_53, 4)
  constantStrand(C, RR_53, 5)
  constantStrand(C, RR_53, 10)

  constantStrands(C, RR_53)  
  constantStrands(C, RR_1000)  
  constantStrands(C, RR_300)  
  kk1 = ZZ/1073741891
  kk2 = ZZ/1073741909
  constantStrands(C, kk1)
  constantStrands(C, kk2)  
  constantStrands(C, ZZ)
  
  R1 = RR_53 (monoid R)
  R2 = RR_1000 (monoid R)
  R3 = kk1 (monoid R)
  R4 = kk2 (monoid R)
  betti'ans = new BettiTally from {(0,{0},0) => 1, (1,{3},3) => 6, (1,{4},4) => 1, (1,{5},5) => 3, (1,{6},6) => 6,
      (2,{4},4) => 1, (2,{5},5) => 3, (2,{6},6) => 22, (2,{7},7) => 29, (2,{8},8) => 9, (3,{6},6) => 1, (3,{7},7)
      => 14, (3,{8},8) => 52, (3,{9},9) => 45, (3,{10},10) => 4, (4,{8},8) => 4, (4,{9},9) => 35, (4,{10},10) =>
      52, (4,{11},11) => 14, (4,{12},12) => 4, (5,{10},10) => 9, (5,{11},11) => 29, (5,{12},12) => 10, (5,{13},13)
      => 3, (5,{14},14) => 1, (6,{12},12) => 6, (6,{13},13) => 3, (6,{14},14) => 1}
  assert(betti'ans == betti (C1 = getNonminimalRes(C, R1)))
  assert(betti'ans == betti (C2 = getNonminimalRes(C, R2)))
  assert(betti'ans == betti (C3 = getNonminimalRes(C, R3)))
  assert(betti'ans == betti (C4 = getNonminimalRes(C, R4)))
  assert(C1.dd^2 == 0)
  assert(C2.dd^2 == 0)
  assert(C3.dd^2 == 0)
  assert(C4.dd^2 == 0)
///

TEST ///
restart
  needsPackage "SVDComplexes"
  
  R = ZZ/32003[a..e]
  I = ideal(a^3, b^3, c^3, d^3, e^3, (a+b+c+d+e)^3)
  C = res(I, FastNonminimal=>true)
  C1 = getNonminimalRes(C, R)
  assert(C == C1)
///
-----------------------------------------------
-- Code for SVD of a complex ------------------
-----------------------------------------------

laplacians = method()
laplacians ChainComplex := (L) -> (
      laps := new MutableHashTable;
      for i from min L to max L do (
	  laps#i=((transpose L.dd_(i))*L.dd_(i) + (L.dd_(i+1) * (transpose L.dd_(i+1)))));
      new HashTable from laps
      )

degreeZeroMatrix = method()
degreeZeroMatrix(ChainComplex, ZZ, ZZ) := (C, slanteddeg, level) -> (
    if ring C === QQ then error "need to provide a target coefficient ring, QQ is not allowed";
    kk := coefficientRing ring C;
    rawC := C.Resolution.RawComputation;
    matrix map(coefficientRing ring C, rawResolutionGetMatrix2(rawC, level, slanteddeg+level))
    )

degreeZeroMatrix(ChainComplex, Ring, ZZ, ZZ) := (C, kk, slanteddeg, level) -> (
    if kk =!= QQ then degreeZeroMatrix(C,slanteddeg, level)
    else (
        rawC := C.Resolution.RawComputation;
        matrix map(kk, rawResolutionGetMutableMatrix2B(rawC, raw kk, slanteddeg+level,level))
        )
    )

-- given a mutable Betti table, find the spots (deg,lev) where there are degree 0 maps.
degzero = (B) -> (
    degsB := select(keys B, (lev,deglist,deg) -> B#?(lev-1,deglist,deg));
    degsB = degsB/(x -> (x#0, x#2-x#0));
    degsB = degsB/reverse//sort -- (deg,lev) pairs.
    )  

numericRank = method()
numericRank(RR, Matrix) := (epsilon, M) -> (
    invEps := 1/epsilon;
    if ring M =!= RR_53 and ring M =!= CC_53 then error "expected a real or complex matrix";
    (sigma, U, Vt) := SVD M;
    pos := select(#sigma-1, i -> sigma#i/sigma#(i+1) > invEps);
    if #pos === 0 then #sigma else min pos+1
    )
numericRank(RR, MutableMatrix) := (epsilon, M) -> (
    invEps := 1/epsilon;
    if ring M =!= RR_53 and ring M =!= CC_53 then error "expected a real or complex matrix";
    (sigma, U, Vt) := SVD M;
    sigma = flatten entries sigma;
    pos := select(#sigma-1, i -> sigma#i/sigma#(i+1) > invEps);
    if #pos === 0 then #sigma else min pos+1
    )
numericRank MutableMatrix :=
numericRank Matrix := (M) -> numericRank(1e-4, M)

minimizeBetti = method()
minimizeBetti(ChainComplex, Ring) := (C, kk) -> (
    B := betti C;
    mB := new MutableHashTable from B;
    rk := if kk =!= RR_53 then rank else numericRank;
    for x in degzero B do (
      (sdeg,lev) := x;
      m := degreeZeroMatrix(C, kk, sdeg, lev);
      r := rk m;
      << "doing " << (sdeg, lev) << " rank[" << numRows m << "," << numColumns m << "] = " << r << endl;
      mB#(lev,{lev+sdeg},lev+sdeg) = mB#(lev,{lev+sdeg},lev+sdeg) - r;
      mB#(lev-1,{lev+sdeg},lev+sdeg) = mB#(lev-1,{lev+sdeg},lev+sdeg) - r;
      if debugLevel > 2 then << "new betti = " << betti mB << endl;
      );
  new BettiTally from mB
  )

commonEntries = method(Options =>{Threshold=>1e-4})
commonEntries(List,List) := opts -> (A,B) -> (
    -- A, B decending list of real numbers
    -- returns list of position, where these numbers coincide up to 4 digits
    Ac:={};Bc:={};
    i:=0;j:=0;
    while (
	    if abs((A_i-B_j)/(A_i+B_j)) < opts.Threshold then (
	        Ac=append(Ac,i);
            Bc=append(Bc,j);
	        i=i+1;j=j+1)
    	else if A_i<B_j then j=j+1 else i=i+1;
    	i<#A and j< #B
        ) do ();
    return(Ac,Bc)
    )

uniquify = method()
uniquify(List,List,RR) := (L1, L2, Threshold) -> (
    -- L1, L2 are lists of floats, sorted in descending order
    -- returns lists L1', L2' also sorted, with no common entries
    M1 := {};
    M2 := {};
    i := 0;
    j := 0;
    while i < #L1 and j < #L2 do (
        a := L1_i;
        b := L2_j;
        << "comparing " << (i,j) << " values: " << (a,b) << endl;
        if abs(a-b)/(abs(a)+abs(b)) < Threshold then (
            i = i+1;
            j = j+1;
            )
        else (
            if a > b then (M1 = append(M1, a); i = i+1)
            else (M2 = append(M2,b); j = j+1)
            )
        );
    return if i == #L1 then (M1,join(M2,L2_{j..#L2-1}))
    else if j == #L2 then (join(M1,L1_{i..#L1-1}), M2)
    else error "damn, our logic is messed up!"
    )


SVDComplex = method(Options => {
        Strategy => Projection, -- other choice: Laplacian,
	Threshold => 1e-4
        }
    )

SVDComplex ChainComplex := opts -> (C) -> (
    if ring C =!= RR_53 then error "excepted chain complex over the reals RR_53";
    goodspots := select(spots C, i -> C_i != 0);
    if #goodspots === 1 then return (id_C, hashTable {goodspots#0 => rank C_(goodspots#0)}, hashTable{});
    (lo, hi) := (min goodspots, max goodspots);
    Cranks := hashTable for ell from lo to hi list ell => rank C_ell;
    rks := new MutableHashTable; -- from lo to hi, these are the ranks of C.dd_ell, with rks#lo = 0.
    hs := new MutableHashTable; -- lo..hi, rank of homology at that step.
    Sigmas := new MutableHashTable; -- the singular values in the SVD complex, indexed lo+1..hi
    Orthos := new MutableHashTable; -- the orthog matrices of the SVD complex, indexed lo..hi
    smallestSing := new MutableHashTable;
    rks#lo = 0;
    sigma1 := null;
    U := null;
    Vt := null;
    if opts.Strategy == symbol Projection then (
        P0 := mutableIdentity(ring C, rank C_lo); -- last projector matrix constructed
        Q0 := mutableMatrix(ring C, 0, Cranks#lo);
        for ell from lo+1 to hi do (
            m1 :=  P0 * (mutableMatrix C.dd_ell); -- crashes if mutable matrices??
            (sigma1, U, Vt) = if numrows m1 > 0 then SVD m1 else 
	    (matrix{{}},matrix{{}},id_(C_ell));
            sigma1 = flatten entries sigma1;
            Sigmas#ell = sigma1;
            -- TODO: the following line needs to be un-hardcoded!!
	    pos := select(#sigma1-1, i -> sigma1#i*opts.Threshold >= sigma1#(i+1));
            rks#ell = if #pos === 0 then #sigma1 else (min pos)+1;
            --remove?-- rks#ell = # select(sigma1, x -> x > 1e-10);
            smallestSing#ell =(if rks#ell-1>0 then sigma1#(rks#ell-2) else null,  sigma1#(rks#ell-1), if rks#ell < #sigma1-1 then sigma1#(rks#ell) else null);
            hs#(ell-1) = Cranks#(ell-1) - rks#(ell-1) - rks#ell;
	    smallestSing#ell = if #sigma1==0 then null else
	    (if rks#ell-1>0 then sigma1#(rks#ell-2) else null,  sigma1#(rks#ell-1), 
		if rks#ell < #sigma1-1 and rks#ell>0 then sigma1#(rks#ell) else null);
            hs#(ell-1) = Cranks#(ell-1) - rks#(ell-1) - rks#ell;
            -- For the vertical map, we need to combine the 2 parts of U, and the remaining part of the map from before
            ortho1 := (transpose U) * P0; 
	    --ortho1 := (transpose U) *matrix P0;
            Orthos#(ell-1) = matrix{{matrix Q0},{matrix ortho1}};
            -- now split Vt into 2 parts.
            P0 = Vt^(toList(rks#ell..numRows Vt-1));
            Q0 = Vt^(toList(0..rks#ell-1));
            );
        -- Now create the Sigma matrices
        Orthos#hi = matrix Vt;
        hs#hi = Cranks#hi - rks#hi;
        SigmaMatrices := hashTable for ell from lo+1 to hi list ell => (
            m := mutableMatrix(RR_53, Cranks#(ell-1), Cranks#ell);
            for i from 0 to rks#ell-1 do m_(rks#(ell-1)+i, i) = Sigmas#ell#i;
            matrix m -- TODO: make this via diagonal matrices and block matrices.
            );
        sourceComplex := (chainComplex SigmaMatrices);
	-- transpose all ortho matrices to get the map in the right direction 
	for i from lo to hi do Orthos#i=transpose Orthos#i;
        result := newChainComplexMap(C, sourceComplex,  new HashTable from Orthos);
        return ( new HashTable from hs,result);--,
	 new HashTable from hs, new HashTable from smallestSing;
        );
    if opts.Strategy == symbol Laplacian then (
	deltas := laplacians C;
	eigVec := new MutableHashTable;
	eigVal := new MutableHashTable;
	commonPositions := new MutableHashTable;
	for ell from lo to hi do (
	    eigVec#ell = eigenvectors(deltas#ell,Hermitian=>true);
	    -- eigenvectors returns the eigenvalue in increasing order --
            eigVal#ell = reverse toList first eigVec#ell);
	posEigVal:= for ell from lo to hi list (select(eigVal#ell,lambda->lambda>0));
	apply(posEigVal,p-> if #unique p != #p then return 
	    ( "Have multiple eigenvalues in a single Laplacian"));
	for ell from lo to hi-1 do (
	    commonPositions#ell = commonEntries(eigVal#ell,eigVal#(ell+1),Threshold=>opts.Threshold));
	for ell from lo+1 to hi-2 do (k:=
		#unique(last commonPositions#ell|(first commonPositions#(ell+1))) == 
		#last commonPositions#ell+#first commonPositions#(ell+1); 
    		if not k then return "Have multiple eigenvalues for Laplacians";);
    	for ell from lo+1 to hi-1 do rks#ell = #first (commonPositions#(ell-1));
	rks#hi = #last commonPositions#(hi-1);
	rks#(hi+1) = 0;
	rks#(lo) = 0; 
    	for ell from lo to hi do hs#ell = Cranks#ell-rks#ell-rks#(ell+1);
	-- now arrange the columns of the orthogonal matrices in three steps
	pos := new MutableHashTable;
     	for ell from lo to hi  do (  
	    pos#ell = if ell == lo then first commonPositions#ell 
    	    else if ell==hi then last commonPositions#(ell-1)
    	    else last commonPositions#(ell-1)| first commonPositions#ell; 
	    leftOver := toList(0..#eigVal#ell-1);
            apply(pos#ell,j->leftOver=delete(j,leftOver));
    	    pos#ell=pos#ell|leftOver;
	    -- a reversal is necessary because eigenvectors returns the eigenvalues in increasing order
    	    cmax:=max pos#ell;
    	    pos#ell=apply(#pos#ell,j->cmax-pos#ell_j));
	for ell from lo to hi do Orthos#ell = (last eigVec#ell)_(pos#ell);
	SigmaMatrices = hashTable for ell from lo+1 to hi list ell => (
	    sigma:= transpose( Orthos#(ell-1)) *C.dd_ell* Orthos#ell;
            m := mutableMatrix(RR_53, Cranks#(ell-1), Cranks#ell);
	    d := mutableIdentity(RR_53, Cranks#(ell));
            for i from 0 to rks#ell-1 do (m_(rks#(ell-1)+i, i) = abs sigma_(rks#(ell-1)+i,i);
		if sigma_(rks#(ell-1)+i,i) < 0 then d_(i,i)=-1);
	    Orthos#ell = Orthos#ell * matrix d;
	    matrix m
            );
	sourceComplex = (chainComplex SigmaMatrices);
	-- do not transpose all ortho matrices to get the map in the right direction 
        -- for i from lo to hi do Orthos#i=transpose Orthos#i;
        U = newChainComplexMap(C, sourceComplex,  new HashTable from Orthos);
	h := new HashTable from hs;
    	return(h,U);
	);
    error "expected Strategy=>Projection or Strategy=>Laplacian"
    )

SVDComplex(ChainComplex,ChainComplex) := opts -> (C,C') -> (
    -- returns a hash table of the ranks of the homology of C
    if ring C =!= RR_53 then error "excepted chain complex over the reals RR_53";
    goodspots := select(spots C, i -> C_i != 0);
    if #goodspots === 1 then return (hashTable {goodspots#0 => rank C_(goodspots#0)}, hashTable{});
    (lo, hi) := (min goodspots, max goodspots);
    if not betti C == betti C' then error "expected two complexes which differ only by their precision";
    Cranks := hashTable for ell from lo to hi list ell => rank C_ell;
    rks := new MutableHashTable; -- from lo to hi, these are the ranks of C.dd_ell, with rks#lo = 0.
    hs := new MutableHashTable; -- lo..hi, rank of homology at that step.
    Sigmas := new MutableHashTable; -- the singular values in the SVD complex, indexed lo+1..hi
    Orthos := new MutableHashTable; -- the orthog matrices of the SVD complex, indexed lo..hi
    smallestSing := new MutableHashTable;
    rks#lo = 0;
    sigma1 := null; sigma1' := null;
    U := null; U' := null;
    Vt := null;Vt' := null;
    if opts.Strategy == symbol Projection then (
        P0 := mutableIdentity(ring C, rank C_lo); -- last projector matrix constructed
        P0' := mutableIdentity(ring C, rank C_lo);
	Q0 := mutableMatrix(ring C, 0, Cranks#lo);
	for ell from lo+1 to hi do (
            m1 := P0 * (mutableMatrix C.dd_ell); -- crashes if mutable matrices??
            (sigma1, U, Vt) = SVD m1;
            sigma1 = flatten entries sigma1;
	    Sigmas#ell = sigma1;
	    m1' := P0' * (mutableMatrix C'.dd_ell); -- crashes if mutable matrices??
            (sigma1', U', Vt') = SVD m1';
            sigma1' = flatten entries sigma1';
            -- TODO: the following line needs to be un-hardcoded!!
            pos := select(#sigma1, i -> abs(sigma1#i-sigma1'#i)/(sigma1#i+sigma1'#i) < opts.Threshold);
            rks#ell = #pos;
            --remove?-- rks#ell = # select(sigma1, x -> x > 1e-10);
            smallestSing#ell =(if rks#ell-1>0 then sigma1#(rks#ell-2) else null,  sigma1#(rks#ell-1), if rks#ell < #sigma1-1 then sigma1#(rks#ell) else null);
            hs#(ell-1) = Cranks#(ell-1) - rks#(ell-1) - rks#ell;
            -- now split Vt into 2 parts.
	    ortho1 := (transpose U) * P0; 
	    --ortho1 := (transpose U) *matrix P0;
            Orthos#(ell-1) = matrix{{matrix Q0},{matrix ortho1}};
            -- now split Vt into 2 parts.
            P0 = Vt^(toList(rks#ell..numRows Vt-1));
	    P0' = Vt'^(toList(rks#ell..numRows Vt-1));
            Q0 = Vt^(toList(0..rks#ell-1));
            );
        -- Now create the Sigma matrices
        Orthos#hi = matrix Vt;
        hs#hi = Cranks#hi - rks#hi;
        SigmaMatrices := hashTable for ell from lo+1 to hi list ell => (
            m := mutableMatrix(RR_53, Cranks#(ell-1), Cranks#ell);
            for i from 0 to rks#ell-1 do m_(rks#(ell-1)+i, i) = Sigmas#ell#i;
            matrix m -- TODO: make this via diagonal matrices and block matrices.
            );
        sourceComplex := (chainComplex SigmaMatrices);
	-- transpose all ortho matrices to get the map in the right direction 
	for i from lo to hi do Orthos#i=transpose Orthos#i;
        result := newChainComplexMap(C, sourceComplex,  new HashTable from Orthos);
        return ( new HashTable from hs,result);--,
	-- new HashTable from hs, new HashTable from smallestSing;
	);   
    if opts.Strategy == symbol Laplacian then error "not implemented for complexes in two precisions";
    )


SVDHomology = method (Options => options SVDComplex)
SVDHomology ChainComplex := opts -> (C) -> (
    -- returns a hash table of the ranks of the homology of C
    if ring C =!= RR_53 then error "excepted chain complex over the reals RR_53";
    goodspots := select(spots C, i -> C_i != 0);
    if #goodspots === 1 then return (hashTable {goodspots#0 => rank C_(goodspots#0)}, hashTable{});
    (lo, hi) := (min goodspots, max goodspots);
    Cranks := hashTable for ell from lo to hi list ell => rank C_ell;
    rks := new MutableHashTable; -- from lo to hi, these are the ranks of C.dd_ell, with rks#lo = 0.
    hs := new MutableHashTable; -- lo..hi, rank of homology at that step.
    smallestSing := new MutableHashTable;
    rks#lo = 0;
    sigma1 := null;
    U := null;
    Vt := null;
    if opts.Strategy == symbol Projection then (
        P0 := mutableIdentity(ring C, rank C_lo); -- last projector matrix constructed
        for ell from lo+1 to hi do (
            m1 := P0 * (mutableMatrix C.dd_ell); -- crashes if mutable matrices??
            (sigma1, U, Vt) = if numrows m1 > 0 then SVD m1 else (matrix{{}},null,id_(C_ell));
            sigma1 = flatten entries sigma1;
            -- TODO: the following line needs to be un-hardcoded!!
            pos := select(#sigma1-1, i -> sigma1#i*opts.Threshold >=sigma1#(i+1));
            rks#ell = if #pos === 0 then #sigma1 else (min pos)+1;
            --remove?-- rks#ell = # select(sigma1, x -> x > 1e-10);
            smallestSing#ell = if #sigma1==0 then null else
	    (if rks#ell-1>0 then sigma1#(rks#ell-2) else null,  sigma1#(rks#ell-1), 
		if rks#ell < #sigma1-1 and rks#ell>0 then sigma1#(rks#ell) else null);
            hs#(ell-1) = Cranks#(ell-1) - rks#(ell-1) - rks#ell;
            -- now split Vt into 2 parts.
            P0 = Vt^(toList(rks#ell..numRows Vt-1));
            );
        hs#hi = Cranks#hi - rks#hi;
        return (new HashTable from hs, new HashTable from smallestSing);
        );
    if opts.Strategy == symbol Laplacian then (
	deltas := laplacians C;
	eigVec := new MutableHashTable;
	eigVal := new MutableHashTable;
	commonPositions := new MutableHashTable;
	for ell from lo to hi do (
	    eigVec#ell = eigenvectors(deltas#ell,Hermitian=>true);
	    -- eigenvectors returns the eigenvalue in increasing order --
            eigVal#ell = reverse toList first eigVec#ell);
		posEigVal:= for ell from lo to hi list (select(eigVal#ell,lambda->lambda>0));
	apply(posEigVal,p-> if #unique p != #p then error "Have multiple eigenvalues in a single Laplacian");
	for ell from lo to hi-1 do (
	    commonPositions#ell = commonEntries(eigVal#ell,eigVal#(ell+1),Threshold=>opts.Threshold));
	for ell from lo+1 to hi-2 do (k:=
		#unique(last commonPositions#ell|(first commonPositions#(ell+1))) == 
		#last commonPositions#ell+#first commonPositions#(ell+1); 
    		if not k then return "Have multiple eigenvalues for Laplacians";);
    	for ell from lo+1 to hi-1 do rks#ell = #first (commonPositions#(ell-1));
	rks#hi = #last commonPositions#(hi-1);
	rks#(hi+1) = 0;
	rks#(lo) = 0;
    	for ell from lo to hi do hs#ell = Cranks#ell-rks#ell-rks#(ell+1);

    	for ell from lo to hi do hs#ell = Cranks#ell-rks#ell-rks#(ell+1);
	for ell from lo to hi do (
	    lamb2:= if ell<hi then (
	    	pos1:=last (first commonPositions#(ell));
	    	(eigVal#(ell))_pos1)
	    else null;
	    lamb1 := if ell > lo then (
	    	pos2:=last (last commonPositions#(ell-1));
	        (eigVal#(ell))_pos2)
	    else null;
	    lamb3:=if hs#ell > 0 then eigVal#(ell)_(Cranks#ell-hs#ell) else null;
	    smallestSing#ell =(lamb1,lamb2,lamb3);
	    );
	 return (new HashTable from hs, new HashTable from smallestSing);
        );
    error "expected Strategy=>Projection or Strategy=>Laplacian"
    )


SVDHomology(ChainComplex,ChainComplex) := opts -> (C,C') -> (
    -- returns a hash table of the ranks of the homology of C
    if ring C =!= RR_53 then error "excepted chain complex over the reals RR_53";
    goodspots := select(spots C, i -> C_i != 0);
    if #goodspots === 1 then return (hashTable {goodspots#0 => rank C_(goodspots#0)}, hashTable{});
    (lo, hi) := (min goodspots, max goodspots);
    if not betti C == betti C' then error "expected two complexes which differ only by their precision";
    Cranks := hashTable for ell from lo to hi list ell => rank C_ell;
    rks := new MutableHashTable; -- from lo to hi, these are the ranks of C.dd_ell, with rks#lo = 0.
    hs := new MutableHashTable; -- lo..hi, rank of homology at that step.
    smallestSing := new MutableHashTable;
    rks#lo = 0;
    sigma1 := null; sigma1' := null;
    U := null; U' := null;
    Vt := null;Vt' := null;
    if opts.Strategy == symbol Projection then (
        P0 := mutableIdentity(ring C, rank C_lo); -- last projector matrix constructed
        P0' := mutableIdentity(ring C, rank C_lo);
	for ell from lo+1 to hi do (
            m1 := P0 * (mutableMatrix C.dd_ell); -- crashes if mutable matrices??
            (sigma1, U, Vt) = SVD m1;
            sigma1 = flatten entries sigma1;
	    m1' := P0' * (mutableMatrix C'.dd_ell); -- crashes if mutable matrices??
            (sigma1', U', Vt') = SVD m1';
            sigma1' = flatten entries sigma1';
            -- TODO: the following line needs to be un-hardcoded!!
            pos := select(#sigma1, i -> abs(sigma1#i-sigma1'#i)/(sigma1#i+sigma1'#i) < opts.Threshold);
            rks#ell = #pos;
            --remove?-- rks#ell = # select(sigma1, x -> x > 1e-10);
            smallestSing#ell =(if rks#ell-1>0 then sigma1#(rks#ell-2) else null,  sigma1#(rks#ell-1), if rks#ell < #sigma1-1 then sigma1#(rks#ell) else null);
            hs#(ell-1) = Cranks#(ell-1) - rks#(ell-1) - rks#ell;
            -- now split Vt into 2 parts.
            P0 = Vt^(toList(rks#ell..numRows Vt-1));
	    P0' = Vt'^(toList(rks#ell..numRows Vt-1));
            );
        hs#hi = Cranks#hi - rks#hi;
        return (new HashTable from hs, new HashTable from smallestSing);
        );
    if opts.Strategy == symbol Laplacian then error "not implemented for complexes in two precisions";
    )


TEST ///
restart
needsPackage "SVDComplexes"
needsPackage "RandomComplexes"

h={1,3,5,2,1} 
r={5,11,3,2}
elapsedTime C=randomChainComplex(h,r,Height=>4)

CR=(C**RR_53)
elapsedTime SVDHomology CR
elapsedTime SVDHomology(CR,Strategy=>Laplacian)
elapsedTime (h,U)=SVDComplex CR;
h
Sigma =source U
Sigma.dd_0
errors=apply(toList(min CR+1..max CR),ell->C.dd_ell-U_(ell-1)*Sigma.dd_ell*transpose U_ell);
maximalEntry chainComplex errors


elapsedTime (h,U)=SVDComplex(CR,Strategy=>Laplacian);
h
SigmaL =source U
maximalEntry(SigmaL.dd_1 -Sigma.dd_1)
errors=apply(toList(min C+1..max C),ell->C.dd_ell-U_(ell-1)*SigmaL.dd_ell*transpose U_ell);
maximalEntry chainComplex errors

///



toBetti = method()
toBetti(ZZ, HashTable) := (deg, H) -> (
      new BettiTally from for k in keys H list (k, {deg}, deg) => H#k
      )

SVDBetti = method()
SVDBetti ChainComplex := (C) -> (
    if coefficientRing ring C =!= QQ then error "expected FastNonminimal resolution over QQ"; 
    Ls := constantStrands(C,RR_53);
    H := hashTable for i in keys Ls list i => SVDHomology Ls#i;
    H2 := hashTable for i in keys H list i => last H#i;
    << "singular values: " << H2 << endl;
    sum for i in keys H list toBetti(i, first H#i)
    )

maxEntry = method()
maxEntry(Matrix) := (m) -> (flatten entries m)/abs//max
maxEntry(ChainComplexMap) := (F) -> max for m in spots F list maxEntry(F_m)

checkSVDComplex = (C, Fhs) -> (
    -- routine to find the smallest errors which occur.
    -- where here (F,hs) = SVDComplex C, C is a complex over RR_53.
    (F,hs, minsing) := Fhs;
    debug Core;
    tar2 := (target F).dd^2;
    src2 := (source F).dd^2;
    val1 := maxEntry tar2;
    val2 := maxEntry src2;
    vals3 := for m in spots F list (flatten entries (((transpose F_m) * F_m) - id_(source F_m)))/abs//max;
    vals4 := for i in spots F list (
        m := (target F).dd_i * F_i - F_(i-1) * (source F).dd_i;
        (flatten entries m)/abs//max
        );
    vals5 := for i in spots F list (
        m := (C.dd_i - ((transpose F_(i-1)) * (target F).dd_i * F_i));
        (flatten entries m)/abs//max
        );
    (val1, val2, vals3, vals4, vals5)
    )

pseudoInverse=method(Options=> options SVDComplex)
pseudoInverse ChainComplex := opts -> C -> (
    U := last SVDComplex(C,Strategy=>opts.Strategy);
    SigmaComplex := source U;
    minC := min C;
    maxC := max C;
    range := toList(minC+1..maxC);
    At := apply(range, i->transpose SigmaComplex.dd_i);
    SigmaPlus := apply(At,A->matrix (apply(numrows A,i->apply(numcols A,j-> 
		    if A_(i,j)==0 then 0 else 1/(A_(i,j))))));
    CplusMats := apply(#SigmaPlus,i->
	     U_(minC+i+1)*SigmaPlus_i* transpose U_(minC+i));
    Cplus := (chainComplex reverse CplusMats);
    Cplus
    )

TEST ///
restart
  needsPackage "SVDComplexes"
  h={1,4,6,5,1} 
  r={1,3,3,4}
  C=randomChainComplex(h,r)
  C = C ** RR_53
  SVDComplex C
  pseudoInverse C
///

conjugateComplex=method(Options=>{Height=>10})
conjugateComplex ChainComplex := opts -> C -> (
    minC:= min C;
    maxC:= max C;  
    U:=for i from minC to maxC list (
	r:=rank C_i;
	randomSL(r,opts));
    C':=for i from minC+1 to maxC list (
	U_(i-1)*C.dd_i*inverse U_i);
    (chainComplex C')[-minC])
    
normalize=method()
normalize ChainComplex := C-> (
    if not ring C === RR_53 then error "expected a complex over RR_53";
    minC:= min C;
    maxC:= max C;  
    C':=for i from minC+1 to maxC list (
	m:=max(flatten entries C.dd_i/abs);
	1/m*C.dd_i);
    chainComplex C'[-minC])

clean(RR, ChainComplex) := (epsilon, C) -> (
    chainComplex hashTable for i from min C + 1 to max C list i => clean(epsilon, C.dd_i)
    )

clean(RR, ChainComplexMap) := (epsilon, f) -> (
    H := hashTable for k in keys f list if instance(k,ZZ) then k => clean(epsilon, f_k) else continue;
    newChainComplexMap(clean(epsilon, target f), clean(epsilon, source f), H)
    )
-- TODO for free res stuff with Frank:
-- add QR
-- make sure code doesn't crash when doing minimalBetti over QQ...
-- allow choice of ZZ/p?
-- 

beginDocumentation()

doc ///
   Key
     SVDComplexes
   Headline
     support for computing homology, ranks and SVD complexes, from a chain complex over the real numbers
   Description
    Text
      Some functionality here should be moved elsewhere.
      
      Here is an example of the usage.
   Caveat
     Currently, this package requires that the Macaulay2 being run is from the res-2107 git branch
///



TEST ///
 needsPackage "RandomComplexes"
  h={1,4,6,5,1} 
  r={1,3,3,4}
  C=randomChainComplex(h,r)
  prune HH C
  assert(C.dd^2 == 0)
  assert(h == for i from 0 to 4 list rank HH_i C)
  assert(r == for i from 1 to 4 list rank(C.dd_i))

  C = randomChainComplex(h={1,6,4,7}, r={1,1,1})
  prune HH C
  assert(C.dd^2 == 0)
  assert(h == for i from 0 to length C list rank HH_i C)
  assert(r == for i from 1 to length C list rank(C.dd_i))

  assert try (C = randomChainComplex({1,6,4,7,4}, {1,1,1}); false) else true

  C = randomChainComplex(h={0,0}, r={10})
  prune HH C
  assert(C.dd^2 == 0)
  assert(h == for i from 0 to length C list rank HH_i C)
  assert(r == for i from 1 to length C list rank(C.dd_i))
  
  C = randomChainComplex(h={1,1},r={0})
  prune HH C
  assert(C.dd_1 == 0)
  assert(C.dd^2 == 0)
  assert(h == for i from 0 to length C list rank HH_i C)
  assert(r == for i from 1 to length C list rank(C.dd_i))
///


TEST ///
  debug needsPackage "SVDComplexes"
  assert(det randomSL 0 == 1)
  assert(det randomSL 1 == 1)
  assert(det randomSL 5 == 1)
  randomSL(10, Height=>100)
  randomSL(10)
  assert((flatten entries randomSL(10, Height=>1000))/abs//max < 1000^2)
  assert((flatten entries randomSL(10, Height=>500))/abs//max < 500^2)
  assert((flatten entries randomSL(10, Height=>10000))/abs//max < 10000^2)
///

doc ///
   Key
     constantStrand
     (constantStrand, ChainComplex, Ring, ZZ)
   Headline
     a constant strand of a chain complex
   Usage
     Cd = constantStrand(C, kk, deg)
   Inputs
     C:ChainComplex
       A chain complex created using {\tt res(I, FastNonminimal=>true)}
     kk:Ring
       if the coefficient ring of the ring of C is QQ, then this should be either:
       RR_53, RR_1000, ZZ/1073741891, or ZZ/1073741909.  
     deg:ZZ
       the degree that one wants to choose.
   Outputs
     Cd:ChainComplex
       a chain complex over {\tt kk}, consisting of the submatrices of {\tt C} of degree {\tt deg}
   Description
    Text
      Warning! This function is very rough currently.  It workes if one uses it in the intended manner,
      as in the example below.  But it should be much more general, handling other rings with grace,
      and also it should handle arbitrary (graded) chain complexes.
    Example
      R = QQ[a..d]
      I = ideal(a^3, b^3, c^3, d^3, (a+3*b+7*c-4*d)^3)
      C = res(I, FastNonminimal=>true)
      betti C
      CR = constantStrand(C, RR_53, 8)
      CR.dd_4
      CR2 = constantStrand(C, RR_1000, 8)
      CR2.dd_4
      kk1 = ZZ/1073741891
      kk2 = ZZ/1073741909
      Cp1 = constantStrand(C, kk1, 8)
      Cp2 = constantStrand(C, kk2, 8)
      (CR.dd_4, CR2.dd_4, Cp1.dd_4, Cp2.dd_4)
      (clean(1e-14,CR)).dd_4
      (clean(1e-299,CR2)).dd_4
    Text
      Setting the input ring to be the integers, although a hack, sets each entry to the 
      number of multiplications used to create this number.  Warning: the result is almost certainly
      not a complex!  This part of this function is experimental, and will likely change
      in later versions.
    Example
      CZ = constantStrand(C, ZZ, 8)
      CZ.dd_4
   Caveat
     This function should be defined for any graded chain complex, not just ones created
     using {\tt res(I, FastNonminimal=>true)}.  Currently, it is used to extract information 
     from the not yet implemented ring QQhybrid, whose elements, coming from QQ, are stored as real number 
     approximations (as doubles, and as 1000 bit floating numbers), together with its remainders under a couple of primes,
     together with information about how many multiplications were performed to obtain this number.
   SeeAlso
     constantStrands
///

TEST ///
  R = QQ[a..d]
  I = ideal(a^3, b^3, c^3, d^3, (a+3*b+7*c-4*d)^3)
  C = res(I, FastNonminimal=>true)
  betti C
  betti'deg8 = new BettiTally from {(3,{},0) => 13, (4,{},0) => 4}
  CR = constantStrand(C, RR_53, 8)
  CR2 = constantStrand(C, RR_1000, 8)

  kk1 = ZZ/1073741891
  kk2 = ZZ/1073741909
  Cp1 = constantStrand(C, kk1, 8)
  Cp2 = constantStrand(C, kk2, 8)

  assert(betti'deg8 == betti CR)
  assert(betti'deg8 == betti CR2)  
  assert(betti'deg8 == betti Cp1)
  assert(betti'deg8 == betti Cp2)
  
  (CR.dd_4, CR2.dd_4, Cp1.dd_4, Cp2.dd_4)
  (clean(1e-14,CR)).dd_4
  (clean(1e-299,CR2)).dd_4
///

TEST ///
  kk = QQ
  R = kk[a..d]
  I = ideal(a^3, b^3, c^3, d^3, (a+3*b+7*c-4*d)^3)
  C = res(I, FastNonminimal=>true)
  betti C
  constantStrand(C, RR_53, 8) -- fails, as it doesn't even make it to that code
///

doc ///
   Key
     numericRank
     (numericRank,Matrix)
     (numericRank,MutableMatrix)
     (numericRank,RR,Matrix)
     (numericRank,RR,MutableMatrix)
   Headline
     approximate rank of a matrix, using SVD
   Usage
     rk = numericRank A
     rk = numericRank(eps, A)
   Inputs
     A:Matrix
       a matrix or a mutable matrix over RR_53 or CC_53
   Outputs
     rk:ZZ
       an approximation to the rank of the matrix A
   Description
    Text
      The singular value decomposition (over RR_53, or CC_53) of the matrix A
      is performed.  If there is a large cutoff in the list of singular values, that
      value separates the zero singular values from the rest, and the number of
      singular values larger than this separating value is called the numeric rank.
    Example
      B = random(RR^30, RR^5);
      C = random(RR^5, RR^30);
      A = B*C;
      numericRank A
      first SVD A
      
      B = mutableMatrix random(RR^100, RR^50);
      C = mutableMatrix random(RR^50, RR^100);
      A = B*C;
      numericRank A

      B = mutableMatrix random(CC^100, CC^50);
      C = mutableMatrix random(CC^50, CC^100);
      A = B*C;
      numericRank A
   Caveat
     The heuristic for determining approximate rank is just that: a heuristic.
     Thus if you really need to make sure the answer is correct or meaningful, 
     you should review the singular values yourself
   SeeAlso
     SVD
///

doc ///
   Key
     SVDComplex
     (SVDComplex,ChainComplex)
     (SVDComplex,ChainComplex,ChainComplex)
   Headline
     Compute the SVD decomposition of a chainComplex over RR
   Usage
     (h,U)=SVDComplex C or
     (h,U)=SVDComplex(C,C')
   Inputs
     C:ChainComplex
       over RR_{53}
     C':ChainComplex
       in a lower precision
   Outputs
     h:HashTable
       the dimensions of the homology groups HH C
     U:ChainComplexMap
       a map C <- Sigma
       where the source is the chainComplex of the singular value matrices
       and U is given by orthogonal matrices  
   Description
    Text
      We compute the singular value decomposition either by the iterated Projections or by the 
      Laplacian method. In case the input consists of two chainComplexes we use the iterated  Projection method, and identify the stable
      singular values.
    Example
      needsPackage "RandomComplexes"
      h={1,3,5,2,1} 
      r={5,11,3,2}
      elapsedTime C=randomChainComplex(h,r,Height=>4)
      C.dd^2
      CR=(C**RR_53)[1]
      elapsedTime (h,U)=SVDComplex CR;
      h
      Sigma =source U
      Sigma.dd_0
      errors=apply(toList(min CR+1..max CR),ell->CR.dd_ell-U_(ell-1)*Sigma.dd_ell*transpose U_ell);
      maximalEntry chainComplex errors

      elapsedTime (hL,U)=SVDComplex(CR,Strategy=>Laplacian);
      hL === h
      SigmaL =source U;
      for i from min CR+1 to max CR list maximalEntry(SigmaL.dd_i -Sigma.dd_i)
      errors=apply(toList(min C+1..max C),ell->CR.dd_ell-U_(ell-1)*SigmaL.dd_ell*transpose U_ell);
      maximalEntry chainComplex errors
    Text
      The optional argument 
   Caveat
      The algorithm might fail if the conditions numbers of the differential are too bad
   SeeAlso
     
///

end--

restart
uninstallPackage "SVDComplexes"
restart
installPackage "SVDComplexes"
viewHelp "SVDComplexes"
restart
check "SVDComplexes"
restart
needsPackage "SVDComplexes"

///
needsPackage "randomComplexes"
needsPackage "SVDComplexes"
needsPackage "AGRExamples"
R=QQ[a..h]
Rp=(ZZ/32003)(monoid R)
Rp1=(ZZ/1073741891)(monoid R)
R0=(RR_53)(monoid R)
deg=4
nextra=10
setRandomSeed "1"
F=sum(gens R,x->x^deg)+sum(nextra,i->(random(1,R))^deg);
elapsedTime I=ideal fromDual matrix{{F}};
elapsedTime C=res(I,FastNonminimal =>true);
--C0 = getNonminimalRes(C, R0);
betti C
elapsedTime minimalBetti sub(I,Rp)
elapsedTime SVDBetti C

            0  1   2   3   4   5   6  7 8
o14 = total: 1 28 105 288 420 288 104 30 4
         0: 1  .   .   .   .   .   .  . .
         1: . 18  42   .   .   .   .  . .
         2: . 10  63 288 420 288  63  9 1
         3: .  .   .   .   .   .  41 19 2
         4: .  .   .   .   .   .   .  2 1
=> does not gives correct values for any choice off the cut off value.
the value here is 1.5e1

debug Core
rawResolutionGetMutableMatrixB(C.Resolution.RawComputation, raw R0, 3);


rawResolutionGetMutableMatrix2B(C.Resolution.RawComputation, raw(ZZ/32003), 3,2)
rawResolutionGetMutableMatrix2B(C.Resolution.RawComputation, raw(RR_53), 3,2)
rawResolutionGetMutableMatrix2B(C.Resolution.RawComputation, raw(ZZ), 9,7)
rawResolutionGetMutableMatrix2B(C.Resolution.RawComputation, raw(RR_53), 9,7)
for i from 1 to 5 list for j from 1 to 5 list
  rawResolutionGetMutableMatrix2B(C.Resolution.RawComputation, raw(RR_53), i,j)
  
Ls = constantStrands(C, RR_53)
Ls1 = constantStrands(C, RR_1000)
m1 = Ls#9 .dd_6;
m2 = Ls1#9 .dd_6;
elapsedTime SVDHomology Ls#9
elapsedTime SVDHomology Ls1#9
first SVD m1, first SVD m2
m1 = Ls#9 .dd_7;
m2 = Ls1#9 .dd_7;
clean(1e-7, m1-m2)
ratios = (L) -> prepend(L#0, for i from 0 to #L-2 list L#i/L#(i+1)) -- L is a sorted list of singular values
ratios first o20
ratios first SVD Ls#11 .dd_8
ratios first SVD Ls#10 .dd_7
ratios first SVD Ls#10 .dd_8
ratios first SVD Ls#9 .dd_6
(a1,a2,a3) = SVDComplex Ls#9;
first SVD Ls#9 .dd_6
first SVD Ls#9 .dd_7
a3
a2
target a1
(source a1).dd_8
(target a1).dd_8
a1_8
Ls#10 .dd_8

laps = laplacians Ls#9;
laps = laplacians Ls#3;
#laps
evs = laps/(m -> rsort eigenvalues(m, Hermitian=>true))

laps1 = laplacians Ls1#9;
evs1 = laps1/(m -> rsort eigenvalues(m, Hermitian=>true))


(M1, M2) = uniquify(evs_0, evs_1, 1e-3);
(M2', M3) = uniquify(M2, evs_2, 1e-3);
#M1
#M2'
#M3

(M1, M2) = uniquify(evs1_0, evs1_1, 1e-3);
(M2', M3) = uniquify(M2, evs1_2, 1e-3);


(M1, M2) = (VerticalList M1, VerticalList M2)
(VerticalList M2', VerticalList M3)
(M1',M2')=uniquify(M1,M2,1e-1)
(#M1', #M2')
sings = (first SVD laps_0, first SVD laps_1, first SVD laps_2)

rsort join(evs_0, evs_2), rsort evs_1
sings = (eigenvalues( laps_0, first SVD laps_1, first SVD laps_2)

rsort join(sings_0, sings_1)
R32009=(ZZ/32009)(monoid R)
minimalBetti sub(I, R32009)


///

TEST ///
  -- warning: this currently requires test code on res2017 branch.
  -- XXXX
restart
  needsPackage "SVDComplexes"
  needsPackage "AGRExamples"
  R = QQ[a..d]
  F = randomForm(3, R)
  I = ideal fromDual matrix{{F}}
  C = res(I, FastNonminimal=>true)

  C.dd -- want this to currently give an error, or make a ring out of this type...
  
  Rp = (ZZ/32003)(monoid R)
  R0 = (RR_53) (monoid R)
  Ls = constantStrands(C,RR_53)  
  L = Ls#3
  Lp = laplacians L
  Lp/eigenvalues
  Lp/SVD/first
  
  Cp = getNonminimalRes(C, Rp)
  C0 = getNonminimalRes(C, R0)
  Cp.dd^2
  C0.dd^2
  -- lcm of lead term entries: 8902598454
  -- want to solve x = y/8902598454^2, where y is an integer, and we know x to double precision
  --  and we know x mod 32003.
  -- example: 
  cf = leadCoefficient ((C0.dd_2)_(9,8))
  -- .293215710985088
  leadCoefficient ((Cp.dd_2)_(9,8))
  -- -10338
  -- what is y? (x mod p) = (y mod p)/(lcm mod p)^2
  kk = coefficientRing Rp
  (-10338_kk) / (8902598454_kk)^2
  -- -391...
  (-391 + 32003*k) / 8902598454^2 == .293215710985088
  (cf * 8902598454^2 + 391)/32003.0
  y = 726156310379351
  (y+0.0)/8902598454^2
  oo * 1_kk
///

TEST ///
  -- warning: this currently requires test code on res2017 branch.
restart
  -- YYYYY
  needsPackage "RandomComplexes"
  needsPackage "SVDComplexes"
  needsPackage "AGRExamples"
  R = QQ[a..f]
  deg = 6
  nextra = 10
  nextra = 20
  nextra = 30
  --F = randomForm(deg, R)
  setRandomSeed "1000"
   F = sum(gens R, x -> x^deg) + sum(nextra, i -> (randomForm(1,R))^deg);
elapsedTime  I = ideal fromDual matrix{{F}};
  C = res(I, FastNonminimal=>true)

  Rp = (ZZ/32003)(monoid R)
 betti res  substitute(I,Rp)
  R0 = (RR_53) (monoid R)
  minimalBetti sub(I, Rp)
  SVDBetti C  

  betti C
  Ls = constantStrands(C,RR_53)  
--  Lp = constantStrands(C,ZZ/32003)  
  D = Ls#8
Ls  
--  (F, hs, minsing) = 
  U=SVDComplex D;
  (hs, minsing) = SVDHomology D;
  hs, minsing
  numericRank D.dd_4

maximalEntry D

  elapsedTime first SVDComplex D
  elapsedTime  SVDHomology( D,Strategy=>Laplacian)
  elapsedTime SVDComplex Ls_5;
  last oo

  hashTable for k in keys Ls list (k => betti Ls#k)
  sumBetti = method()
  sumBetti HashTable := H -> (
      for k in keys H list (betti H#k)(-k)
      )

  elapsedTime hashTable for i in keys Ls list i => SVDComplex Ls#i;
  
  elapsedTime hashTable for i in keys Ls list i => toBetti(i, first SVDHomology Ls#i);

      
  for i from 0 to #Ls-1 list 
    max flatten checkSVDComplex(Ls_i, SVDComplex Ls_i)

  hashTable for i from 0 to #Ls-1 list 
    i => last SVDComplex Ls_i

  ------ end of example above
    
  debug Core
  kk = ZZp(32003, Strategy=>"Flint")
  Rp = kk(monoid R)
  R0 = (RR_53) (monoid R)
  Cp = getNonminimalRes(C,Rp)
  C0 = getNonminimalRes(C,R0)

  minimizeBetti(C, kk)
  minimizeBetti(C, RR_53)

  Ip = sub(I,Rp);
  minimalBetti Ip

  Lps = constantStrands(C,kk)
  netList oo
  L = Ls_3
  Lp = laplacians L;
  --Lp/eigenvalues

  SVDComplex L
  
  -- compute using projection method the SVD of the complex L
  L.dd_2
  (sigma, U1, V1t) = SVD mutableMatrix L.dd_2
  sigma
  
  betti U1
  betti V1t
  M = mutableMatrix L.dd_2
  sigma1 = mutableMatrix diagonalMatrix matrix sigma
  sigma1 = flatten entries sigma
  sigmaplus = mutableMatrix(RR_53, 75, 5)
  for i from 0 to 4 do sigmaplus_(i,i) = 1/sigma1#i
  sigmaplus
  Mplus = (transpose V1t) * sigmaplus * (transpose U1)
  pkerM = submatrix(V1t, 5..74,);
  M2 = pkerM * mutableMatrix(L.dd_3);
  (sigma2,U2,V2t) = SVD M2  
  sigma2 = flatten entries sigma2
  nonzerosing = position(0..#sigma2-2, i -> (sigma2#(i+1)/sigma2#i < 1.0e-10))
  pkerM2 = submatrix(V2t, nonzerosing+1 .. numRows V2t-1,)  
  sigma2_{0..49}
  sigma2_50  
  M3 = pkerM2 * mutableMatrix(L.dd_4)  ;
  (sigma3,U3,V3t) = SVD M3
  sigma3 = flatten entries sigma3
  nonzerosing3 = position(0..#sigma3-2, i -> (sigma3#(i+1)/sigma3#i < 1.0e-10))
  sigma3#-1 / sigma3#-2 < 1.0e-10
    
  evs = Lp/SVD/first
  loc = 2
  vals = sort join(for a in evs#loc list (a,loc), for a in evs#(loc+1) list (a,loc+1))
  for i from 0 to #vals-2 list (
      if vals_i_1 != vals_(i+1)_1 then (
          abs(vals_i_0 - vals_(i+1)_0) / (vals_i_0 + vals_(i+1)_0), vals_i, vals_(i+1)
          )
      else null
      )      
  errs = select(oo, x -> x =!= null)
  netList oo
  select(errs, x -> x#0 < .1) -- 66
    select(errs, x -> x#0 < .01) -- 50 
    select(errs, x -> x#0 < .001) -- 47
  Cp = getNonminimalRes(C, Rp)
  C0 = getNonminimalRes(C, R0)
  Cp.dd^2
  C0.dd^2 -- TODO: make it so we can "clean" the results here.
///

TEST ///
restart
  needsPackage "SVDComplexes"
  needsPackage "AGRExamples"
  I = getAGR(6,9,50,0);
  R = ring I
  elapsedTime C = res(I, FastNonminimal=>true)

  betti C
  elapsedTime SVDBetti C  

  Rp = (ZZ/32003)(monoid R)
  Ip = ideal sub(gens I, Rp);
  elapsedTime minimalBetti Ip
  elapsedTime Cp = res(Ip, FastNonminimal=>true)
///

TEST ///
restart
  -- ZZZZ
  needsPackage "SVDComplexes"
  needsPackage "AGRExamples"

  I = value get "agr-6-7-37-0.m2";
  makeAGR(6,7,50,0)
  
  I = getAGR(6,7,50,0);
{*  
  R = QQ[a..h]
  deg = 6
  nextra = 30
  F = sum(gens R, x -> x^deg) + sum(nextra, i -> (randomForm(1,R))^deg);
  elapsedTime I = ideal fromDual matrix{{F}};
*}
  
  elapsedTime C = res(I, FastNonminimal=>true)
  betti C
  elapsedTime SVDBetti C  

  Rp = (ZZ/32003)(monoid R)
  Ip = ideal sub(gens I, Rp);
  elapsedTime minimalBetti Ip
  
  D = constantStrand(C, RR_53, 7)
  SVDComplex D;
  E = target first oo
  for i from 2 to 5 list sort flatten entries compress flatten E.dd_i
  Ls = constantStrands(C, RR_53)
///

TEST ///
restart
  needsPackage "SVDComplexes"
  needsPackage "AGRExamples"

  elapsedTime makeAGR(7,7,100,32003)
  I = getAGR(7,7,100,32003);

  elapsedTime minimalBetti I
    
///

TEST ///
  -- warning: this currently requires test code on res2017 branch.
  -- XXXX
restart
  needsPackage "SVDComplexes"
  R = QQ[a..g]
  deg = 6
  nextra = 10
  nextra = 30
  --F = randomForm(deg, R)
  F = sum(gens R, x -> x^deg) + sum(nextra, i -> (randomForm(1,R))^deg);
  elapsedTime I = ideal fromDual matrix{{F}};
  elapsedTime C = res(I, FastNonminimal=>true)

  kk = ZZ/32003
  Rp = kk(monoid R)
  Ip = sub(I,Rp);
  elapsedTime minimalBetti Ip
  R0 = (RR_53) (monoid R)

  Ls = constantStrands(C,RR_53)  
  netList oo
  Lps = constantStrands(C,kk)
  debug Core
  kkflint = ZZp(32003, Strategy=>"Ffpack")
  Lps = constantStrands(C,kkflint)
  Lp = Lps_5
  L = Ls_5
  for i from 3 to 6 list elapsedTime first SVD L.dd_i  
  for i from 3 to 6 list rank mutableMatrix Lp.dd_i
  Lp = laplacians L;
  --Lp/eigenvalues
  evs = Lp/SVD/first
  loc = 2
  vals = sort join(for a in evs#loc list (a,loc), for a in evs#(loc+1) list (a,loc+1))
  for i from 0 to #vals-2 list (
      if vals_i_1 != vals_(i+1)_1 then (
          abs(vals_i_0 - vals_(i+1)_0) / (vals_i_0 + vals_(i+1)_0), vals_i, vals_(i+1)
          )
      else null
      )      
  errs = select(oo, x -> x =!= null)
  netList oo
  select(errs, x -> x#0 < .1) -- 66
    select(errs, x -> x#0 < .01) -- 50 
    select(errs, x -> x#0 < .001) -- 47
  Cp = getNonminimalRes(C, Rp)
  C0 = getNonminimalRes(C, R0)
  Cp.dd^2
  C0.dd^2 -- TODO: make it so we can "clean" the results here.
///


TEST ///
  -- warning: this currently requires test code on res2017 branch.
  -- XXXX
restart
  needsPackage "SVDComplexes"
  needsPackage "AGRExamples"
  deg = 6
  nv = 7
  nextra = binomial(nv + 1, 2) - nv - 10
  R = QQ[vars(0..nv-1)]


  --F = randomForm(deg, R)
  F = sum(gens R, x -> x^deg) + sum(nextra, i -> (randomForm(1,R))^deg);
  elapsedTime I = ideal fromDual matrix{{F}};
  elapsedTime C = res(I, FastNonminimal=>true)

  kk = ZZ/32003
  Rp = kk(monoid R)
  Ip = sub(I,Rp);
  elapsedTime Cp = res(Ip, FastNonminimal=>true)
  elapsedTime minimalBetti Ip
  R0 = (RR_53) (monoid R)
  SVDBetti C
  
  Ls = constantStrands(C,RR_53)  
  mats = flatten for L in Ls list (
      kf := keys L.dd;
      nonzeros := select(kf, k -> instance(k,ZZ) and L.dd_k != 0);
      nonzeros/(i -> L.dd_i)
      );
  elapsedTime(mats/(m -> first SVD m))
  netList oo
  Lps = constantStrands(C,kk)
  debug Core
  kkflint = ZZp(32003, Strategy=>"Ffpack")
  Lps = constantStrands(C,kkflint)
  Lp = Lps_5
  L = Ls_5
  for i from 3 to 6 list rank mutableMatrix Lp.dd_i
  Lp = laplacians L;
  --Lp/eigenvalues
  evs = Lp/SVD/first
  loc = 2
  vals = sort join(for a in evs#loc list (a,loc), for a in evs#(loc+1) list (a,loc+1))
  for i from 0 to #vals-2 list (
      if vals_i_1 != vals_(i+1)_1 then (
          abs(vals_i_0 - vals_(i+1)_0) / (vals_i_0 + vals_(i+1)_0), vals_i, vals_(i+1)
          )
      else null
      )      
  errs = select(oo, x -> x =!= null)
  netList oo
  select(errs, x -> x#0 < .1) -- 66
    select(errs, x -> x#0 < .01) -- 50 
    select(errs, x -> x#0 < .001) -- 47
  Cp = getNonminimalRes(C, Rp)
  C0 = getNonminimalRes(C, R0)
  Cp.dd^2
  C0.dd^2 -- TODO: make it so we can "clean" the results here.
///


doc ///
Key
  SVDComplexes
Headline
Description
  Text
  Example
Caveat
SeeAlso
///

doc ///
Key
Headline
Usage
Inputs
Outputs
Consequences
Description
  Text
  Example
  Code
  Pre
Caveat
SeeAlso
///

TEST ///
-- test code and assertions here
-- may have as many TEST sections as needed
///
