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
    "constantStrand",
    "constantStrands",
    "laplacians",
    "getNonminimalRes",
    "degreeZeroMatrix",
    "minimizeBetti",
    "SVDComplex",
    "SVDHomology",
    "SVDBetti",
    "Projection",
    "Laplacian",
    "newChainComplexMap",
    "numericRank",
    "checkSVDComplex"
}

-----------------------------------------------
-- Code for SVD of a complex ------------------
-----------------------------------------------
debug Core
constantStrand = method()
constantStrand(ChainComplex, Ring, ZZ) := (C, kk, deg) -> (
    -- base ring of C should be QQ
    if coefficientRing ring C =!= QQ then error "ring of the complex must be a polynomial ring over QQ";
    -- assumption: we are resolving an ideal, or at least all gens occur in degree >= 0.
    len := length C;
    reg := regularity C;
    --if deg <= 2 or deg > len+reg then error("degree should be in the range 2.."|len+reg);
    chainComplex for lev from 1 to len list (
        matrix map(kk, rawResolutionGetMutableMatrix2B(C.Resolution.RawComputation, raw kk, deg,lev))
        )
    )    

constantStrands = method()
constantStrands(ChainComplex, Ring) := (C, kk) -> (
    -- base ring of C should be QQ
    if coefficientRing ring C =!= QQ then error "ring of the complex must be a polynomial ring over QQ";
    -- assumption: we are resolving an ideal, or at least all gens occur in degree >= 0.
    len := length C;
    reg := regularity C;
    hashTable for deg from 0 to len+reg list (
        D := constantStrand(C,kk,deg);
        if D == 0 then continue else deg => D
        )
    )

laplacians = method()
laplacians ChainComplex := (L) -> (
      rg := select(spots L, i -> L_i != 0);
      for i in rg list ((transpose L.dd_(i)) *  L.dd_(i) + (L.dd_(i+1) * (transpose L.dd_(i+1))))
      )

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
      if i > 0 then result#i = map(source result#(i-1),,result#i);
      );
    chainComplex toList result
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
numericRank Matrix := (M) -> (
    if ring M =!= RR_53 then error "expected real matrix";
    (sigma, U, Vt) := SVD M;
    pos := select(#sigma-1, i -> sigma#i/sigma#(i+1) > 1e4);
    if #pos === 0 then #sigma else (min pos)+1
    --# select(sigma, s -> s > 1e-10)
    )

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
SVDComplex = method(Options => {
        Strategy => Projection -- other choice: Laplacian
        }
    )

SVDComplex ChainComplex := opts -> (C) -> (
    if ring C =!= RR_53 then error "excepted chain complex over the reals RR_53";
    goodspots := select(spots C, i -> C_i != 0);
    if #goodspots === 1 then return (id_C, hashTable {goodspots#0 => rank C_(goodspots#0)}, hashTable{});
    (lo, hi) := (min goodspots, max goodspots);
    Cranks := hashTable for ell from lo to hi list ell => rank C_ell;
    rks := new MutableList; -- from lo to hi, these are the ranks of C.dd_ell, with rks#lo = 0.
    hs := new MutableHashTable; -- lo..hi, rank of homology at that step.
    Sigmas := new MutableList; -- the singular values in the SVD complex, indexed lo+1..hi
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
            m1 := P0 * (mutableMatrix C.dd_ell); -- crashes if mutable matrices??
            (sigma1, U, Vt) = SVD m1;
            sigma1 = flatten entries sigma1;
            Sigmas#ell = sigma1;
            -- TODO: the following line needs to be un-hardcoded!!
            rks#ell = # select(sigma1, x -> x > 1e-10);
            smallestSing#ell = sigma1#(rks#ell-1);
            hs#(ell-1) = Cranks#(ell-1) - rks#(ell-1) - rks#ell;
            -- For the vertical map, we need to combine the 2 parts of U, and the remaining part of the map from before
            ortho1 := (transpose U) * P0;
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
        targetComplex := (chainComplex SigmaMatrices);
        result := newChainComplexMap(targetComplex, C, new HashTable from Orthos);
        return (result, new HashTable from hs, new HashTable from smallestSing);
        );
    if opts.Strategy == symbol Laplacian then (
        );
    error "expected Strategy=>Projection or Strategy=>Laplacian"
    )

SVDHomology = method (Options => options SVDComplex)
SVDHomology ChainComplex := opts -> (C) -> (
    -- returns a hash table of the ranks of the homology of C
    if ring C =!= RR_53 then error "excepted chain complex over the reals RR_53";
    goodspots := select(spots C, i -> C_i != 0);
    if #goodspots === 1 then return (hashTable {goodspots#0 => rank C_(goodspots#0)}, hashTable{});
    (lo, hi) := (min goodspots, max goodspots);
    Cranks := hashTable for ell from lo to hi list ell => rank C_ell;
    rks := new MutableList; -- from lo to hi, these are the ranks of C.dd_ell, with rks#lo = 0.
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
            (sigma1, U, Vt) = SVD m1;
            sigma1 = flatten entries sigma1;
            -- TODO: the following line needs to be un-hardcoded!!
            pos := select(#sigma1-1, i -> sigma1#i/sigma1#(i+1) > 1e4);
            rks#ell = if #pos === 0 then #sigma1 else (min pos)+1;
            --remove?-- rks#ell = # select(sigma1, x -> x > 1e-10);
            smallestSing#ell = (sigma1#(rks#ell-1), if rks#ell < #sigma1-1 then sigma1#(rks#ell) else null);
            hs#(ell-1) = Cranks#(ell-1) - rks#(ell-1) - rks#ell;
            -- now split Vt into 2 parts.
            P0 = Vt^(toList(rks#ell..numRows Vt-1));
            );
        hs#hi = Cranks#hi - rks#hi;
        return (new HashTable from hs, new HashTable from smallestSing);
        );
    if opts.Strategy == symbol Laplacian then (
        );
    error "expected Strategy=>Projection or Strategy=>Laplacian"
    )

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
debug Core  
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
-- TODO for free res stuff with Frank:
-- add QR
-- make sure code doesn't crash when doing minimalBetti over QQ...
-- allow choice of ZZ/p?
-- 

beginDocumentation()

end--

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
  R0 = (RR_53) (monoid R)
  minimalBetti sub(I, Rp)
  SVDBetti C  

  betti C
  Ls = constantStrands(C,RR_53)  
  Lp = constantStrands(C,ZZ/32003)  
  D = Ls#7
  
  (F, hs, minsing) = SVDComplex D;
  (hs, minsing) = SVDHomology D;
  hs, minsing
  numericRank D.dd_4

  elapsedTime SVDComplex Ls_4;
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

