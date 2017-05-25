newPackage(
        "NonminimalComplexes",
        Version => "0.1", 
        Date => "",
        Authors => {{Name => "Frank Schreyer", 
                  Email => "", 
                  HomePage => ""},
              {Name => "Mike Stillman", 
                  Email => "", 
                  HomePage => ""}},
        Headline => "Obtaining the non-minimal strands of a non-minimal resolution of a homogeneous module",
        DebuggingMode => true
        )

export {
    -- These functions should be moved.  Where to?
    "constantStrand", -- documented, tests
    "constantStrands", -- documented, tests
    "getNonminimalRes",
    "degreeZeroMatrix",
    "minimizeBetti",
    "SVDBetti"
}

debug Core
needsPackage "SVDComplexes"

-- The following shuold be where?
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
{*  
  restart
  needsPackage "NonminimalComplexes"
*}
  
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
  kk1 = ZZ/32003
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
{*  
  restart
  needsPackage "NonminimalComplexes"
*}
  R = ZZ/32003[a..e]
  I = ideal(a^3, b^3, c^3, d^3, e^3, (a+b+c+d+e)^3)
  C = res(I, FastNonminimal=>true)
  C1 = getNonminimalRes(C, R)
  assert(C == C1)
///

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

toBetti = method()
toBetti(ZZ, HashTable) := (deg, H) -> (
      new BettiTally from for k in keys H list (k, {deg}, deg) => H#k
      )

{*
-- How to handle this here??
SVDBetti = method()
SVDBetti ChainComplex := (C) -> (
    if coefficientRing ring C =!= QQ then error "expected FastNonminimal resolution over QQ"; 
    Ls := constantStrands(C,RR_53);
    H := hashTable for i in keys Ls list i => SVDHomology Ls#i;
    H2 := hashTable for i in keys H list i => last H#i;
    << "singular values: " << H2 << endl;
    sum for i in keys H list toBetti(i, first H#i)
    )
*}

beginDocumentation()

doc ///
   Key
     NonminimalComplexes
   Headline
     support for computing homology, ranks and SVD complexes, from a chain complex over the real numbers
   Description
    Text
      Some functionality here should be moved elsewhere.
      
      Here is an example of the usage.
   Caveat
     Currently, this package requires that the Macaulay2 being run is from the res-2107 git branch
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
      kk1 = ZZ/32003
      kk2 = ZZ/1073741909
      Cp1 = constantStrand(C, kk1, 8)
      Cp2 = constantStrand(C, kk2, 8)
      netList {{CR.dd_4, CR2.dd_4}, {Cp1.dd_4, Cp2.dd_4}}
      (clean(1e-14,CR)).dd_4
      netList {(clean(1e-14,CR)).dd_4}==netList {(clean(1e-299,CR2)).dd_4}
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

doc ///
   Key
     constantStrands
     (constantStrands, ChainComplex, Ring)
   Headline
     all constant strands of a chain complex
   Usage
     Cs = constantStrands(C, kk)
   Inputs
     C:ChainComplex
       A chain complex created using {\tt res(I, FastNonminimal=>true)}
     kk:Ring
       if the coefficient ring of the ring of C is QQ, then this should be either:
       RR_{53}, RR_{1000}, ZZ/1073741891, or ZZ/1073741909.  
     deg:ZZ
       the degree that one wants to choose.
   Outputs
     Cs:List
      the list of chain complex over {\tt kk}, which for each degree degree {\tt deg}, consisting of the submatrices of {\tt C} of degree {\tt deg}
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
      Cs = constantStrands(C, RR_53)
      CR=Cs#8         
   Caveat
     This function should be defined for any graded chain complex, not just ones created
     using {\tt res(I, FastNonminimal=>true)}.  Currently, it is used to extract information 
     from the not yet implemented ring QQhybrid, whose elements, coming from QQ, are stored as real number 
     approximations (as doubles, and as 1000 bit floating numbers), together with its remainders under a couple of primes,
     together with information about how many multiplications were performed to obtain this number.
   SeeAlso
     constantStrand
///

TEST ///
  R = QQ[a..d]
  I = ideal(a^3, b^3, c^3, d^3, (a+3*b+7*c-4*d)^3)
  C = res(I, FastNonminimal=>true)
  betti C
  betti'deg8 = new BettiTally from {(3,{},0) => 13, (4,{},0) => 4}
  CR = constantStrand(C, RR_53, 8)
  CR2 = constantStrand(C, RR_1000, 8)

  kk1 = ZZ/32003
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
  constantStrand(C, RR_53, 8)
   -- fails, as it doesn't even make it to that code
///


end--
{*
Cs2 = (constantStrands(C, RR_1000))#8
      kk1 = ZZ/32003
      kk2 = ZZ/1073741909
      Cp1 = (constantStrands(C, kk1))#8
      Cp2 =(constantStrands(C, kk2))#8
      CR.dd_4, CR2.dd_4
      Cp1.dd_4, Cp2.dd_4   
      netList {{CR.dd_4, CR2.dd_4}, {Cp1.dd_4, Cp2.dd_4}}
      netList{(clean(1e-14,CR)).dd_4,(clean(1e-299,CR2)).dd_4}
      netList {(clean(1e-14,CR)).dd_4} == netList{(clean(1e-299,CR2)).dd_4}
      *}

restart
uninstallPackage "NonminimalComplexes"
restart

installPackage "NonminimalComplexes"
viewHelp "NonminimlaComplexes"
restart
check "SVDComplexes"
restart
needsPackage "NonminimalComplexes"

///
needsPackage "RandomComplexes"
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
C0 = getNonminimalRes(C, R0);
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
elapsedTime SVDHomology(Ls1#9**RR_53,Ls#9)
elapsedTime SVDHomology (Ls1#9**RR_53,Strategy=>Laplacian,Threshold=>1e-2)
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
