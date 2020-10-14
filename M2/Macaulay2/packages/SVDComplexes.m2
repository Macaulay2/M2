///
restart
uninstallPackage "SVDComplexes"
installPackage "SVDComplexes"
loadPackage("SVDComplexes",Reload=>true)
check("SVDComplexes", UserMode=>true)
viewHelp "SVDComplexes"
///


newPackage(
        "SVDComplexes",
        Version => "0.3", 
        Date => "May 23, 2018",
        Authors => {
            {Name => "Frank-Olaf Schreyer", 
		        Email => "schreyer@math.uni-sb.de",
		        HomePage => "http://www.math.uni-sb.de/ag/schreyer/"},
	        {Name => "Mike Stillman", 
                Email => "mike@math.cornell.edu", 
                HomePage => "http://www.math.cornell.edu/~mike"}
            },
        Headline => "SVD (singular value decomposition) of a complex over the reals and related functions",
	Keywords => {"Homological Algebra", "Commutative Algebra"},
	PackageExports => {"LLLBases"},
        DebuggingMode => false
        )

export {
    -- These functions should be placed into M2 itself.
    "newChainComplexMap",
    -- Keep the ones below this line
    "laplacians",
    "SVDComplex",
    "SVDHomology",
    "pseudoInverse",
    "pseudoInverse1",-- temporarily exported
    "projectToComplex",
    "euclideanDistance",
    "Projection",
    "Laplacian",
    "numericRank",
    "commonEntries",
    "checkSVDComplex",
    "arePseudoInverses"
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


///
-*
  no assertion
  restart
  needsPackage "SVDComplexes"
*-
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
-*
  restart
  needsPackage "SVDComplexes"
*-

needsPackage "RandomComplexes"

h={1,1,1,1}
r={2,2,2}
setRandomSeed 2
C=randomChainComplex(h,r,Height=>9,WithLLL=>true,ZeroMean=>true)
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
pseudoInverse ChainComplex := opts -> C -> ( -- old version, to be removed.
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

pseudoInverse ChainComplex := opts -> C -> (
    if not ring C === RR_53 then pseudoInverse1 C else (
    U := last SVDComplex(C,Strategy=>opts.Strategy);
    SigmaComplex := source U;
    (loC,hiC) := concentration C;
    At := for i from loC to hiC list (-i) => transpose SigmaComplex.dd_i;
    CPlusMats := hashTable for i from loC+1 to hiC list (
        A := transpose SigmaComplex.dd_i;
        eA := entries A;
        sigmaPlus := matrix for r in eA list for a in r list if a == 0 then 0 else 1/a;
        (-i+1) => U_i * sigmaPlus * transpose U_(i-1)
        );
    chainComplex CPlusMats
    ))

pseudoInverse1=method()
    
pseudoInverse1 ChainComplex := C -> (
    if not isField ring C then error " expected a chain complex defined over a field";
    (lo,hi) := concentration C;
    chainComplex hashTable for i from lo to hi list (-i+1) => pseudoInverse1 C.dd_i
    )

pseudoInverse1(Matrix) := M -> (
    -- BUG: this sometimes has P1 or P2 == 0, in which case Mplus, P, Q are all 0, and so the assertions fail.
    -- note for FS: I'll track this down and fix it (MS)
    if not isField ring M then error " expected a matrix defined over a field";
    if M==0 then return transpose M;
    rk := rank M;
    m := numRows M;
    n:= numColumns M;
    -- now find an rxr non-zero minar 
    -- need to be improved    
    Lm:=subsets(0..m-1,rk);
    Pm:=Lm_(position(Lm,c->rank M^c==rk));
    A:= M^Pm;
    Ln:=subsets(0..n-1,rk);
    Pn:=Ln_(position(Ln,c->rank A_c==rk));
    B:= (M_Pn);
    inj:=(id_(source M))_Pn;
    P1:= ((transpose B)*B)^(-1)*transpose B;
    P2:= transpose A*(A *transpose A)^(-1)* A;
    Mplus:= P2*inj *P1;
    P:= M*Mplus;
    Q:= Mplus*M;
    assert(Mplus*P==Mplus);
    assert(M*Q==M);
    return Mplus)    

arePseudoInverses = method(Options=>{Threshold=>1e-10})
arePseudoInverses(Matrix,Matrix) := opts -> (A,B) -> (
    -- see https://en.wikipedia.org/wiki/Moore%E2%80%93Penrose_pseudoinverse
    if ring A =!= ring B then error "expected rings over the same field";
    if not isField ring A and not instance(ring A, InexactField) then error "expected ring to be a field, e.g. ZZ/p, QQ,RR, or CC";
    if not (isFreeModule source A and isFreeModule source B and isFreeModule target A and isFreeModule target B) then (
        error "expected matrices between free modules";
        );
    if numRows A != numColumns B or numColumns A != numRows B then (
        if debugLevel > 0 then << "expected matrices of sizes (m,n) and (n,m)" << endl;
        return false;
        );
    diff1 := A*B*A-A;
    diff2 := B*A*B-B;
    diff3 := A*B - transpose (A*B);
    diff4 := B*A - transpose (B*A);    
    if instance(ring A, InexactField) then (
        norm1 := norm diff1; -- this 'norm' is infinity norm: max (abs) value of the entries of diff1.
        norm2 := norm diff2;
        norm3 := norm diff3;
        norm4 := norm diff4;
        eps := opts.Threshold;
        if norm1 > eps then (
            if debugLevel > 0 then << "A*B*A != A, abs max value of an entry of difference is " << norm1 << endl;
            return false;
            );
        if norm2 > eps then (
            if debugLevel > 0 then << "B*A*B != B, abs max value of an entry of difference is " << norm2 << endl;
            return false;
            );
        if norm3 > eps then (
            if debugLevel > 0 then << "A*B != transpose(A*B), abs max value of an entry of difference is " << norm3 << endl;
            return false;
            );
        if norm4 > eps then (
            if debugLevel > 0 then << "A*B != transpose(A*B), abs max value of an entry of difference is " << norm4 << endl;
            return false;
            );
        )
    else (
        if diff1 != 0 then (
            if debugLevel > 0 then << "A*B*A != A, difference is " << diff1 << endl;
            return false;
            );
        if diff2 != 0 then (
            if debugLevel > 0 then << "B*A*B != B, difference is " << diff2 << endl;
            return false;
            );
        if diff3 != 0 then (
            if debugLevel > 0 then << "A*B != transpose(A*B), difference is " << diff3 << endl;
            return false;
            );
        if diff4 != 0 then (
            if debugLevel > 0 then << "A*B != transpose(A*B), difference is " << diff4 << endl;
            return false;
            );
        );
    true
    )

arePseudoInverses(ChainComplex, ChainComplex) := opts -> (A,B) -> (
    (loA,hiA) := concentration A;
    (loB,hiB) := concentration B;
    if loA != -hiB or loB != -hiA then (
        if debugLevel > 0 then << "expected chain complexes with dual indices" << endl;
        return false;
        );
    for i from loA+1 to hiA do (
        if not arePseudoInverses(A.dd_i, B.dd_(-i+1)) then return false; 
        );
    true
    )

TEST ///
-*
  restart
  needsPackage "SVDComplexes"
*-

  needsPackage "RandomComplexes"
  -- Simple boundary cases for psuedoInverse.
  m = matrix id_(QQ^2)
  C = chainComplex {m}
  CRR = chainComplex {m ** RR_53}
  C3 = C[-3]
  iC = pseudoInverse C
  iC3 = pseudoInverse C3
  icRR = pseudoInverse CRR 
///
    
TEST ///
-*
  restart
  debug needsPackage "SVDComplexes"
*-

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
  CQi = pseudoInverse CQ
  M=CQ.dd_2 
  pseudoInverse1 M
  isField RR_53
  Ci2 = CQ[1]
  assert(Ci2.dd^2 == 0)
  Ci3 = pseudoInverse( CQ[1])
  assert(Ci3.dd^2 == 0)

  kk = ZZ/32003  
  CF = CZ ** kk
  CFi = pseudoInverse CF
  assert(CF.dd^2 == 0)
  assert arePseudoInverses(CF, CFi)
  
  assert arePseudoInverses(C.dd_1, Ci.dd_0)
  assert arePseudoInverses(C.dd_2, Ci.dd_(-1))
  assert arePseudoInverses(C.dd_3, Ci.dd_(-2))
  assert arePseudoInverses(C.dd_4, Ci.dd_(-3))
  assert arePseudoInverses(C.dd_5, Ci.dd_(-4))

  assert arePseudoInverses(CQ.dd_1, CQi.dd_0)
  assert arePseudoInverses(CQ.dd_2, CQi.dd_(-1))  
  assert arePseudoInverses(CQ.dd_3, CQi.dd_(-2))
  assert arePseudoInverses(CQ.dd_4, CQi.dd_(-3))
  assert arePseudoInverses(CQ.dd_5, CQi.dd_(-4))

  assert arePseudoInverses(CQ, CQi)
  assert arePseudoInverses(C, Ci)
///

-*
TEST ///
  -- pseudo inverses do not exist over finite fields sometimes:
  kk = ZZ/5
  M = matrix{{2,1},{1,-2}} ** kk
  assert (try (pseudoInverse1 M; false) else true)
  R = kk[a,b,c,d]
  N = matrix{{a,b},{c,d}}
  I = ideal(M*N*M - M) + ideal(N*M*N-N) + ideal(M*N - transpose(M*N))
  assert(I == 1)

  J = ideal(M*N*M - M) + ideal(N*M*N-N)
  gens gb J
  decompose J
  -- Frank's turn
  needsPackage "RandomComplexes"
  C=randomChainComplex({2,3},{2},Height=>10,ZeroMean=>true)
  M=C.dd_1
 
  
  pseudoInverse1 (M**QQ)
  factor 435112
  kk=ZZ/137
  pseudoInverse1 (M**kk)
break
  transpose A*A
  det  ( transpose B*B)  
  (try (pseudoInverse1 N; false) else true)

 p=23;
  kk=ZZ/p;
  elapsedTime tally apply(10*p^2,c->(
  C=randomChainComplex({3,3},{2},Height=>10,ZeroMean=>true);
  M=C.dd_1;
  N=M**kk;
  t=(try (pseudoInverse1 N; true) else false);   
  a=rank source gens  intersect(ker N,image transpose N);
  b=rank source gens  intersect(ker transpose N, image N);
  if not t and a==0 and b==0 then print toString N;
  (t,a,b))
  )

M=matrix {{5, 0, 7, 4, -8}, {0, 0, 0, 0, 0}, {8, 2, -8, -8, 1}, {5, -4, 8, -8, -5}}
Mplus
P^2==P
M*Mplus*M,M
Lm
Ln
Pm,Pn
///
*-

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
    
beginDocumentation()

doc ///
   Key
     SVDComplexes
   Headline
     support for computing homology, ranks and SVD complexes, for a chain complex over the real numbers
   Description
    Text
      This package implements the algorithms in the paper "Singular value decomposition of complexes", by D. Brake, J. Hauenstein, F. Schreyer, A. Sommese, and M. Stillman, 
      @HREF "https://arxiv.org/abs/1804.09838"@.

      Singular value decompositions of matrices are extremely useful in practice.  In particular, an SVD can often reveal the rank (numeric rank) of a matrix.  
      
      In the above paper, we extend the notion of singular value decomposition from matrices over the reals or complexes to a complex of matrices over the reals or complexes.
      
      For some applications,
      one obtains a complex over the (approximate) reals, and one would like to know what the ranks of the matrices are, and therefore the ranks of the homology groups.
      One way to do this would be to compute the SVD of each matrix separately, often revealing the desired ranks.  This is less than satisfactory however, as it ignores the fact
      that this sequence is an approximation of a complex, i.e. each two consecutive matrices multiply to zero.
      
      In this package, and the referenced paper, we give 2 algorithms for computing the SVD of a complex, and the resulting putative ranks of the matrices or ranks of the 
      homology groups.  

      Here is an example of the usage.  We construct a random chain complex whose homology modules have ranks 1, 4, 6, 5, and 1:
    Example
      needsPackage "RandomComplexes"
      h = {1,4,6,5,1} 
      r = {1,3,3,4}
      C = randomChainComplex(h,r)
      CQ = C ** QQ
      prune HH CQ
      CR = C ** RR_53
      (h,U) = SVDComplex CR
    Text
      {\tt U} is a map from the SVD complex of C, to C.  h is a HashTable whose values are the (putative) ranks of the homology groups.
      Note that the entries of the matrices of this complex are the singular values, but they are not on the main diagonal.
    Example
      source U
      (source U).dd
   Caveat
     The algorithms in this package work well in many cases, but it would be nice if a numeric analyst would improve the algorithms!
   SeeAlso
     SVDComplex
     SVDHomology
///



TEST ///
-*
  restart
  needsPackage "RandomComplexes"
*-
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
       a matrix or a mutable matrix over RR_{53} or CC_{53}
     eps: RR
       a relative threshold for consecutive singular values, if not present then the default value 1e-4 is taken
   Outputs
     rk:ZZ
       an approximation to the rank of the matrix A
   Description
    Text
      The singular value decomposition (over RR_{53}, or CC_{53}) of the matrix A
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
     [SVDComplex,Strategy]
     [SVDComplex,Threshold]
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
     Strategy => Symbol 
       Laplacian or Projection for the method used
     Threshold => RR
       the relative threshold used to detect the zero singular values
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
      Laplacian method. In case the input consists of two chainComplexes we use the iterated  
      Projection method, and identify the stable singular values.
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
      The algorithm might fail if the condition numbers of the differential are too bad
   SeeAlso
     
///

doc ///
   Key
     SVDHomology
     (SVDHomology,ChainComplex)
     (SVDHomology,ChainComplex,ChainComplex)
     [SVDHomology, Strategy]
     [SVDHomology,Threshold]
   Headline
     Estimate the homology of a chainComplex over RR with the SVD decomposition
   Usage
     (h,h1)=SVDHomology C or
     (h,h1)=SVDHomology(C,C')
   Inputs
     C:ChainComplex
       over RR_{53}
     C':ChainComplex
       in a lower precision
     Strategy => Symbol 
       Laplacian or Projection for the method used
     Threshold => RR
       the relative threshold used to detect the zero singular values
   Outputs
     h:HashTable
       the dimensions of the homology groups HH C
     h1:HashTable
       information about the singular values 
   Description
    Text
      We compute the singular value decomposition either by the iterated Projections or by the 
      Laplacian method.
      In case of the projection method we record in h1 the last two nonzero singular values and first singular value expected to be really zero.
      
      In case of the Laplacian method we record in h1 the smallest common Eigenvalues
      of the neighboring Laplacians, and the first Eigenvalue expected to be zero. 
      
      In case the input consists of two chainComplexes we use the iterated  Projection method, and identify the stable
      singular values.
    Example
      needsPackage "RandomComplexes"
      h={1,3,5,2} 
      r={4,3,3}
      elapsedTime C=randomChainComplex(h,r,Height=>5,ZeroMean=>true)
      C.dd^2
      CR=(C**RR_53)
      elapsedTime (h,h1)=SVDHomology CR
      elapsedTime (hL,hL1)=SVDHomology(CR,Strategy=>Laplacian)
      hL === h
      (h1#1_1)^2, hL1#1_0, (h1#1_1)^2-hL1#1_0
      (h1#2_1)^2, hL1#2_0, (h1#2_1)^2-hL1#2_0
      (h1#3_1)^2, hL1#3_0, (h1#3_1)^2-hL1#3_0
      -- the squares of the singular values are Eigenvalues of the Laplacians!
      D=disturb(C,1e-3,Strategy=>Discrete)
      C.dd_1
      D.dd_1
      (hd,hd1)=SVDHomology(CR,D,Threshold=>1e-2)
      hd === h
      hd1 === h1 
   Caveat
      The algorithm might fail if the condition numbers of the differential are too bad
   SeeAlso
      SVDComplex
  
///

doc ///
   Key
     projectToComplex
     (projectToComplex,ChainComplex,HashTable)
   Headline
     compute a nearby complex with the projection method
   Usage
     C = projectToComplex(D,h) or
   Inputs
     D:ChainComplex
       an approximate complex over over RR_{53}
     h:HashTable
       the desired homology groups
   Outputs
     C:ChainComplex
       a nearby chainComplex
   Description
    Text
      Using the iterated projection method we compute a nearby chainComplex C with 
      homology h.
    Example
      needsPackage "RandomComplexes"
      setRandomSeed "a good example";
      h={2,3,5,2} 
      r={4,3,3}
      elapsedTime C=randomChainComplex(h,r,Height=>5,ZeroMean=>true)
      C.dd^2
      CR=(C**RR_53)
      h=(SVDHomology CR)_0
      D=disturb(C,1e-2,Strategy=>Discrete)
      C.dd_1
      D.dd_1
      D.dd^2
      C'=projectToComplex(D,h)
      C'.dd^2
      euclideanDistance(C',D)
      euclideanDistance(CR,D)
      euclideanDistance(C',CR)
      Dd=dual D 
      Dd[1]
      hd=(SVDHomology((dual CR)[1]))_0
      C''=(dual projectToComplex(Dd[1],hd))[1]
      C''.dd_1,C'.dd_1
      euclideanDistance(CR,D), euclideanDistance(C'',D), euclideanDistance(C',D)
   Caveat
      The algorithm does not produces the closest 
      nearby complex in the euclidean norm. Instead it is a reminder to develope
      such function. 
   SeeAlso
      SVDComplex
///


doc ///
   Key
     arePseudoInverses
     (arePseudoInverses,Matrix,Matrix)
     (arePseudoInverses,ChainComplex,ChainComplex)
   Headline
     check the Penrose relations for the pseudo inverse  
   Usage
     arePseudoInverses(A,B)
     arePseudoInverses(C,Cplus)      
   Inputs
     A:Matrix
     B:Matrix
       or  
     C:ChainComplex
     Cplus:ChainComplex
     Threshold => RR
       an absolute error up to which the identities should hold
   Outputs
      :Boolean
   Description
    Text
      The functions returns true, if the Penrose relations 
      A*B*A == A, B*A*B == B, A*B==transpose(A*B) and B*A=transpose(B*A)) are satisfied.
      In case of an inexact field the relation should hold up to a threshold.
      In case of two chain complexes, these relation should hold for each pair of pseudo inverse differentials.
    Example
      needsPackage "RandomComplexes"
      setRandomSeed "a pretty good example";
      h={2,2} 
      r={3}
      C=randomChainComplex(h,r,Height=>100,ZeroMean=>true)
      C.dd
      CQ=C**QQ
      CR=C**RR
      CRplus = pseudoInverse CR
      arePseudoInverses(CR,CRplus,Threshold=>1e-10)
      arePseudoInverses(CR,CRplus,Threshold=>1e-1000)
      CQplus = pseudoInverse CQ
      CRplus.dd
      CQplus.dd
      (CQplus**RR_53).dd
      arePseudoInverses(CQ,CQplus)
      Fp=ZZ/nextPrime 10^3
      Cp=C**Fp
      Cpplus=pseudoInverse Cp
      Cpplus.dd
      arePseudoInverses(CQ,CQplus)
      arePseudoInverses(Cp,Cpplus)
   Caveat
      Over finite fields our algorithm to compute the pseudo inverse might fail.
      Hence we need this test to check the assertion.  
   SeeAlso
      pseudoInverse
///

doc ///
   Key
     pseudoInverse
     --(pseudoInverse,Matrix)
     (pseudoInverse,ChainComplex)
   Headline
     compute the pseudoInverse of a chainComplex 
   Usage
     Cplus = pseudoInverse C 
   Inputs
     C:ChainComplex
       an approximate complex over an field 
     Strategy => Symbol 
       Laplacian or Projection for the method used
     Threshold => RR
       the relative threshold used to detect the zero singular values
   Outputs
     Cplus:ChainComplex
       the pseudo inverse complex
   Description
    Text
      In case the field is RR we use the SVD normal form to compute the pseudo inverse.
      In case of QQ we compute the pseudo inverse directly over QQ.
    Example
      needsPackage "RandomComplexes"
      setRandomSeed "a pretty good example";
      h={2,3,1} 
      r={2,3}
      C=randomChainComplex(h,r,Height=>11,ZeroMean=>true)
      C.dd
      CQ=C**QQ
      CR=C**RR_53
      CRplus = pseudoInverse CR
      CQplus = pseudoInverse CQ
      CRplus.dd
      CQplus.dd
      (CQplus**RR_53).dd
      CRplus.dd^2
      CQplus.dd^2
    Text
      Pseudo inverses frequently exist also over finite fields.
    Example        
      Fp=ZZ/nextPrime 10^3
      Cp=C**Fp
      Cpplus=pseudoInverse Cp
      Cpplus.dd
      Cpplus.dd^2
      arePseudoInverses(Cp,Cpplus)
   Caveat
      Over finite fields the algorithm can fail.
   SeeAlso
      arePseudoInverses
      SVDComplex
///

doc ///
   Key
     euclideanDistance     
     (euclideanDistance,ChainComplex,ChainComplex)
   Headline
     compute the euclidean distance of two chain complexes
   Usage
     euclideanDistance(C,D) 
   Inputs
     C:ChainComplex
     D:ChainComplex 
       two chain complexes over RR or QQ   
   Outputs
      :RR
   Description
    Text
      Compute the distance in the L^2-norm of two complexes viewed as a
      sequence of matrices
    Example
      needsPackage "RandomComplexes"
      setRandomSeed "a good example";
      h={2,3,5,2} 
      r={4,3,3}
      elapsedTime C=randomChainComplex(h,r,Height=>5,ZeroMean=>true)
      C.dd^2
      CR=(C**RR_53)
      h=(SVDHomology CR)_0
      D=disturb(C,1e-2,Strategy=>Discrete)
      C.dd_1
      D.dd_1
      D.dd^2
      C'=projectToComplex(D,h)
      C'.dd^2
      euclideanDistance(C',D)
      euclideanDistance(CR,D)
      euclideanDistance(C',CR)
   Caveat
      
   SeeAlso
      projectToComplex
///

doc ///
   Key
     commonEntries     
     (commonEntries,List,List)
   Headline
     lists of positions, where they coincide up to threshold
   Usage
     (P1,P2)=commonEntries(L1,L2)
   Inputs
     L1:List
     L2:List 
       descending lists of non-negative real numbers
     Threshold => RR
       relative error allowed for equality  
   Outputs
      P1:List
      P2:List
   Description
    Text
      Determine the positions, where the non-zero numbers in both lists which coincide up to a threshold.
      This is needed in the Laplacian method to compute the SVD normal form of a complex
    Example
      needsPackage "RandomComplexes"
      setRandomSeed "a good example";
      h={2,3,5,3} 
      r={4,3,5}
      elapsedTime C=randomChainComplex(h,r,Height=>100,ZeroMean=>true)
      C.dd^2
      D=disturb(C**RR_53,1e-4)
      Delta=laplacians D;
      L0=(SVD Delta#0)_0, L1=(SVD Delta#1)_0,L2=(SVD Delta#2)_0,L3=(SVD Delta#3)_0
      commonEntries(L0,L1)
      commonEntries(L1,L2)
      commonEntries(L2,L3)
   Caveat
      
   SeeAlso
      laplacians
      SVDComplex
///

doc ///
   Key
     laplacians     
     (laplacians,ChainComplex)
   Headline
     compute the laplacians of a chain complex
   Usage
     delta=laplacians C
   Inputs
     C:ChainComplex
       defined over RR  
   Outputs
      delta:HashTable
        of the laplacians
   Description
    Text
      For a chain complex over RR defined by matrices A_i=C.dd_i
      the i-th laplacian is defined by
      delta#i = transpose(A_i)*A_i+A_{i+1}*transpose A_{i+1}.
    Example
      needsPackage "RandomComplexes"
      setRandomSeed "a good example";
      h={2,3,5,3} 
      r={4,3,5}
      C=randomChainComplex(h,r,Height=>100,ZeroMean=>true)
      C.dd^2
      D=disturb(C**RR_53,1e-4)
      delta=laplacians D
      L0=(SVD delta#0)_0, L1=(SVD delta#1)_0,L2=(SVD delta#2)_0,L3=(SVD delta#3)_0
      commonEntries(L0,L1)
      commonEntries(L1,L2)
      commonEntries(L2,L3)
   Caveat
      
   SeeAlso
      commonEntries
      SVDComplex
///

doc ///
   Key
    Laplacian
   Headline
    Option for SVDHomology and SVDComplex
   Description
    Text
     Strategy for the computation of the SVD normal form of a complex
///

doc ///
   Key
    Projection
   Headline
    Option for SVDHomology and SVDComplex
   Description
    Text
     Strategy for the computation of the SVD normal form of a complex
///
end--

restart
uninstallPackage "SVDComplexes"
restart
installPackage "SVDComplexes"
viewHelp "SVDComplexes"
restart
loadPackage "SVDComplexes"
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

TEST ///
      needsPackage "RandomComplexes"
      setRandomSeed "a pretty good example";
      h={2,3,1} 
      r={2,3}
      C=randomChainComplex(h,r,Height=>11,ZeroMean=>true)
      C.dd
      CQ=C**QQ
      CR=C**RR_53
      CRplus = pseudoInverse CR
      CQplus = pseudoInverse CQ
      CRplus.dd
      CQplus.dd
      CRplus.dd^2
      CQplus.dd^2
      Fp=ZZ/nextPrime 10^3
      Cp=C**Fp
      Cpplus=pseudoInverse Cp
      Cpplus.dd
      arePseudoInverses(CR,CRplus)
      arePseudoInverses(CQ,CQplus)
      arePseudoInverses(Cp,Cpplus)
      Fp=ZZ/nextPrime 20
      Cp=C**Fp
      Cpplus=pseudoInverse Cp
      M=Cp.dd_2
      Mplus=pseudoInverse1 M
      kerPerp=image syz transpose gens ker M
      intersect(ker M,kerPerp) 
      imPerp= image syz transpose M
      intersect(image M,imPerp) 
///
