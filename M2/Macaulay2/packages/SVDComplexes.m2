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
    -- These functions should be placed into M2 itself.
    "newChainComplexMap",
    "spots",
    -- Keep the ones below this line
    "laplacians",
    "SVDComplex",
    "SVDHomology",
    "pseudoInverse",
    "pseudoInverse1",
    "projectToComplex",
    "euclideanDistance",
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

concentration = method()
concentration ChainComplex := C -> (
    goodspots := select(spots C, i -> C_i != 0);
    if #goodspots == 0 then (0,0) else (min goodspots, max goodspots)
    )

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

clean(RR, ChainComplex) := (epsilon, C) -> (
    chainComplex hashTable for i from min C + 1 to max C list i => clean(epsilon, C.dd_i)
    )

clean(RR, ChainComplexMap) := (epsilon, f) -> (
    H := hashTable for k in keys f list if instance(k,ZZ) then k => clean(epsilon, f_k) else continue;
    newChainComplexMap(clean(epsilon, target f), clean(epsilon, source f), H)
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
-- Code for SVD of a complex ------------------
-----------------------------------------------

laplacians = method()
laplacians ChainComplex := (L) -> (
      laps := new MutableHashTable;
      for i from min L to max L do (
	  laps#i=((transpose L.dd_(i))*L.dd_(i) + (L.dd_(i+1) * (transpose L.dd_(i+1)))));
      new HashTable from laps
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
    (lo, hi) := concentration C;
    if lo === hi then (
        return (id_C, hashTable {lo => rank C_lo}, hashTable{});
        );
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
    (lo, hi) := concentration C;
    if lo === hi then return (hashTable {lo => rank C_lo}, hashTable{});
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
    (lo, hi) := concentration C;
    if lo === hi then return (hashTable {lo => rank C_lo}, hashTable{});
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
    (lo, hi) := concentration C;
    if lo === hi then return (hashTable {lo => rank C_lo}, hashTable{});
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
{*
  restart
  needsPackage "SVDComplexes"
*}
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

projectToComplex=method()
projectToComplex(ChainComplex,HashTable) := (B,hs) -> (
     -- returns a hash table of the ranks of the homology of C
    if ring B =!= RR_53 then error "excepted chain complex over the reals RR_53";
    (lo, hi) := concentration B;
    Cranks := hashTable for ell from lo to hi list ell => rank B_ell;
    rks := new MutableHashTable; 
    rks#lo = 0;
    for ell from lo to hi do (
	rks#(ell+1) = Cranks#ell-hs#ell-rks#ell;
	if rks#ell <0 then error "Rank conditions cannot be satisfied");
    if rks#(hi+1) !=0 then error "Rank conditions cannot be satisfied";
    Sigmas := new MutableHashTable; -- the singular values in the SVD complex, indexed lo+1..hi
    Orthos := new MutableHashTable; -- the orthog matrices of the SVD complex, indexed lo..hi
    sigma1 := null;
    U := null;
    Vt := null;
    P0 := mutableIdentity(ring B, Cranks#lo); -- last projector matrix constructed
    Q0 := mutableMatrix(ring B, 0, Cranks#lo);
    for ell from lo+1 to hi do (
            m1 :=  P0 * (mutableMatrix B.dd_ell); -- crashes if mutable matrices??
            (sigma1, U, Vt) = if numrows m1 > 0 then SVD m1 else 
	    (matrix{{}},matrix{{}},id_(B_ell));
            sigma1 = flatten entries sigma1;
            Sigmas#ell = sigma1;
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
     SigmaMatrices := hashTable for ell from lo+1 to hi list ell => (
            m := mutableMatrix(RR_53, Cranks#(ell-1), Cranks#ell);
            for i from 0 to rks#ell-1 do m_(rks#(ell-1)+i, i) = Sigmas#ell#i;
            matrix m -- TODO: make this via diagonal matrices and block matrices.
            );
	-- transpose all ortho matrices to get the map in the right direction 
     for i from lo to hi do Orthos#i=transpose Orthos#i;
     As := hashTable for ell from lo+1 to hi list ell => (
	    Orthos#(ell-1)*SigmaMatrices#ell* transpose Orthos#ell);
     return chainComplex As)

euclideanDistance=method()
euclideanDistance(ChainComplex,ChainComplex) := (A,B) -> (
    (lo,hi) := (min A, max A);
    if (lo,hi) != (min B,max B) then error "expect complexes of the same range";
    for i from lo to hi do if  A_i =!= B_i then  error "expected complexe with free modules of the same ranks";
    d:=  sum for i from lo+1 to hi list sum( flatten entries (A.dd_i-B.dd_i),c->c^2);
    d^1/2
    )

TEST ///
{*
  restart
  needsPackage "SVDComplexes"
*}

needsPackage "RandomComplexes"

h={1,1,1,1}
r={2,2,2}
setRandomSeed 2
C=randomChainComplex(h,r,Height=>9,WithLLL=>true,zeroMean=>true)
prune HH C
--signes in dual
(dual C).dd_0, transpose C.dd_1
(dual C).dd_(-1), transpose C.dd_2
dual (dual C[1])[1]== C 
dual dual C == C

CR=C**RR_53
B=disturb(CR,1e-3)
euclideanDistance(B,CR)
for i from 1 to 3 list maximalEntry(B.dd_i-CR.dd_i)
SVDHomology B

hs = hashTable {0 =>1,1=>1,2=>1,3=>1}; 
A=projectToComplex(B,hs);
euclideanDistance(A,B)
euclideanDistance(A,CR)
Ad=dual A[1]
(hs,c)=SVDHomology Ad
Bd=dual B[1]
dual (dual B[1])[1]==B
Ad1=projectToComplex(Bd,hs)
A1= dual Ad1[1]
euclideanDistance(A1,A)
euclideanDistance(A1,B)
euclideanDistance(A,B)
for i from 1 to 3 list (B.dd_i-A.dd_i)
for i from 1 to 3 list (B.dd_i-A1.dd_i)
hs = hashTable {0 =>1,1=>2,2=>2,3=>1}; 
A=projectToComplex(B,hs);
euclideanDistance(A,B)
for i from 1 to 3 list maximalEntry(B.dd_i-A.dd_i)
hs = hashTable {0 =>1,1=>0,2=>0,3=>1}; 
A=projectToComplex(B,hs);
euclideanDistance(A,B)
for i from 1 to 3 list maximalEntry(B.dd_i-A.dd_i)

hs = hashTable {0 =>1,1=>1,2=>0,3=>0}; 
A=projectToComplex(B,hs);
euclideanDistance(A,B)
for i from 1 to 3 list maximalEntry(B.dd_i-A.dd_i)
hs = hashTable {0 =>2,1=>2,2=>1,3=>1}; 
A=projectToComplex(B,hs);
euclideanDistance(A,B)
for i from 1 to 3 list maximalEntry(B.dd_i-A.dd_i)


hs = hashTable {0 =>2,1=>1,2=>1,3=>2}; 
A=projectToComplex(B,hs);
for i from 1 to 3 list maximalEntry(B.dd_i-A.dd_i)
///
        
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
    if not ring C === RR_53 then pseudoInverse1 C else (
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
    ))

pseudoInverse1=method()
    
pseudoInverse1(ChainComplex) := C-> (
    if not isField ring C then error " expected a chain complex defined over a field";
    a := min C;
    b := max C;
    Cplus := new ChainComplex;
    for i from -b to -a do Cplus_i= C_(-i);
    for i from -b+1 to -a do Cplus.dd_i = pseudoInverse1( C.dd_(-i+1));
    Cplus
    )

pseudoInverse1(Matrix) := M -> (
    if not isField ring M then error " expected a matrix defined over a field";
    if M==0 then return transpose M;
    rk := rank M;
    m := numRows M;
    n:= numColumns M;
    -- now find an rxr non-zero minar 
    -- need to be improved    
    Lm:=subsets(0..m-1,rk);
    Pm:=Lm_(position(Lm,c->rank M_c==rk));
    A:= M_Pm;
    Ln:=subsets(0..n-1,rk);
    Pn:=Ln_(position(Ln,c->rank A^c==rk));
    B:= transpose M^Pn;
    inj:=(id_(source M))_Pn;
    P1:= ((transpose A)*A)^(-1)*transpose A;
    P2:= B*((transpose B)*B)^(-1)*transpose B;
    Mplus:= P2*inj *P1;
    P:= M*Mplus;
    Q:= Mplus*M;
    assert(Mplus*P==Mplus);
    assert(M*Q==M);
    return Mplus)    
    
TEST ///
{*
  restart
  needsPackage "SVDComplexes"
*}

  needsPackage "RandomComplexes"
  h={1,4,6,5,1} 
  r={1,3,3,4}
  CZ=randomChainComplex(h,r)
  C = CZ ** RR_53
  SVDComplex C
  Ci = pseudoInverse C
  assert(clean(1e-10,Ci.dd^2) == 0)
  Ci1 = pseudoInverse (C[1])
  assert(clean(1e-10,Ci1.dd^2) == 0)
  CQ = CZ**QQ
  M=CQ.dd_2 
  pseudoInverse1 M
  isField RR_53
  Ci2 = CQ[1]
  assert(Ci2.dd^2 == 0)
  Ci3 = pseudoInverse( CQ[1])
  assert(Ci3.dd^2 == 0)

  kk = ZZ/32003  
  CF = CZ ** kk
  assert(CF.dd^2 == 0) -- doesn't always have to happen...  every now and then, this will be false
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
///



TEST ///
{*
  restart
  needsPackage "RandomComplexes"
*}
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
check("SVDComplexes", UserMode=>true)
restart
needsPackage "SVDComplexes"

end--
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
