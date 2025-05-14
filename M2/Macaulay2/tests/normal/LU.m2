-- Tests of LUdecomposition, reducedRowEchelonForm
-- and related functions

----------------------------------------------
-- Useful functions to help test LU, rref functions
-- Some of these routines are taken from the EngineTests package
----------------------------------------------
checkLU = method()
checkLU(List,Matrix,Matrix) := (P,L,U) -> (
     R := ring L;
     Q = id_(R^#P) _ P;
     --Q = mutableMatrix(R, numrows L, numrows L);
     --for i from 0 to numrows L - 1 do Q_(i,P_i) = 1_R;
     --Q = matrix Q;
     Q*L*U)
checkLU Matrix := (M) -> assert (checkLU time LUdecomposition M == M)

permutationMatrix = (p) -> id_(ZZ^#p) _ p
isPermutation = (p) -> (
    -- p is a list of integers of length len.  Determine if it is a rearrangement of 0..len-1
    return(sort p == toList(0..#p - 1))
    )
isUnitLowerTriangular = method()
isUnitLowerTriangular MutableMatrix := L -> (
    m := numrows L;
    if m != numcols L then return false;
    for i from 0 to m-1 do if L_(i,i) != 1 then return false;
    for i from 0 to m-2 do for j from i+1 to m-1 do if L_(i,j) != 0 then return false;
    true
    )
isUnitLowerTriangular Matrix := L -> isUnitLowerTriangular mutableMatrix L
randomFullRank = (R,n) -> (
    S := mutableMatrix(R,n,n);
    for i from 0 to n-1 do for j from 0 to n-1 do S_(i,j) = random 100;
    --fillMatrix S;
    while det S == 0 do (
        S = mutableMatrix(R,n,n);
        fillMatrix S;
        );
    (S, inverse S)
    --Sinv := map(R, rawLinAlgInverse raw S);
    --(S, Sinv)
    )
makeMatrixWithColumnRankProfile = (R, nrows, ncols, prof) -> (
    -- prof is a list of column indices (so each entry should be in range [0,ncols-1])
    --A := first randomFullRank(R, nrows);
    B := mutableMatrix(R, nrows, ncols);
    -- now set B with random values
    for p from 0 to #prof-1 do B_(p,prof#p) = 1_R;
    rk := 1;
    prof = append(prof, ncols);
    for i from 1 to #prof - 1 do (
        -- set all entries in rows 0..rk in columns prof#(i-1) to prof#i-1 to random values
        for r from 0 to rk-1 do for c from prof#(i-1)+1 to prof#i - 1 do B_(r,c) = random R;
        rk = rk+1;
        );
    --A*B
    --(A,B)
    B
    )
pivotColumns = method()
pivotColumns MutableMatrix := List => U -> (
    for i from 0 to numrows U - 1 list (
        piv := null;
        for j from 0 to numcols U - 1 do if U_(i,j) != 0 then (piv = j; break);
        piv
        )
    )

isREF = method()
isREF MutableMatrix := Boolean => U -> (
    -- U is m x n.
    -- check:
    --  a. all zero rows appear after all nonzero rows
    --  b. the leftmost non-zero's (pivots) occur in strictly left -> right order
    pivotcols := pivotColumns U;
    for i from 0 to #pivotcols - 2 do (
        if pivotcols#i === null and pivotcols#(i+1) =!= null then
            return false
        else if pivotcols#(i+1) =!= null and pivotcols#i >= pivotcols#(i+1) then return false;
        );
    true
    )
isREF Matrix := Boolean => U -> isREF mutableMatrix U
isRREF = method()
isRREF MutableMatrix := Boolean => U -> (
    pivcols := pivotColumns U;
    if not isREF U then return false;
    for i from 0 to #pivcols - 1 do (
        c := pivcols#i;
        if c === null then continue;
        -- need to check that the ones above the pivot are 0
        for j from 0 to i-1 do if U_(j, c) != 0 then return false;
        -- need to check that the pivot is 1.
        if U_(i, c) != 1 then return false; -- might need a range, if over RR or CC...?
        );
    true
    )
isRREF Matrix := Boolean => U -> isRREF mutableMatrix U
--------------------------------------------------

-- Test of RREF, LU for matrices over QQ
R = QQ
B = matrix makeMatrixWithColumnRankProfile(R, 5, 10, {1,3,4,7})
A = random(R^5, R^5)
P = permutationMatrix {0,2,3,4,1}
P^-1
M = A * P^-1 * B
assert isRREF reducedRowEchelonForm M
(Q,L,U) = LUdecomposition M
  -- what all to check?
  -- make 2 functions for this? One over approximate fields, one over exact fields...
  assert((permutationMatrix Q) * L * U == M)
  assert isUnitLowerTriangular mutableMatrix L
  assert isREF U


M = matrix {{1.0, 3.0, 4.0, 5.0},{2.0, 3.0, 0.0, 1.0}}
--M = mutableMatrix(M, Dense=>true)
-- isRREF reducedRowEchelonForm M -- not defined yet
(P,L,U) = LUdecomposition M
  assert((permutationMatrix P) * L * U == M)
  assert isUnitLowerTriangular L
  assert isREF U

-- over a prime finite field
kk = ZZ/101
M = random(kk^20, kk^30)
(P,L,U) = LUdecomposition M
  assert((permutationMatrix P) * L * U == M)
  assert isUnitLowerTriangular L
  assert isREF U

-- over a nonprime finite field.  Working!
kk = GF(8)
M = random(kk^20, kk^30)
(P,L,U) = LUdecomposition M
  assert((permutationMatrix P) * L * U == M)
  assert isUnitLowerTriangular L
  assert isREF U

-- over QQ.  Now working (possibly slow...)
kk = QQ
M = random(kk^20, kk^30)
(P,L,U) = LUdecomposition M
  assert((permutationMatrix P) * L * U == M)
  assert isUnitLowerTriangular L
  assert isREF U


-- Not real tests here:
kk = ZZ/32003
M = mutableMatrix(kk,3,3)
fillMatrix M
(P,L,U) = LUdecomposition M
L*U
M
kk = ZZ/32003
M = mutableMatrix(kk,2,2)
fillMatrix M
(P,L,U) = LUdecomposition M
L*U

kk = ZZ/32003
M = matrix(kk, {{3,2},{3,5}})
(P,L,U) = LUdecomposition M
M == L*U

kk = ZZ/32003
M = matrix(kk, {{3,2,1},{3,5,4},{1,1,7}})
(P,L,U) = LUdecomposition M
M == L*U

kk = ZZ/32003
M1 = matrix(kk, {{2,3,1,0,0,1},
            {0,0,1,2,3,1},
            {0,0,0,0,1,1},
            {0,0,0,0,0,0}})
S = random(kk^4, kk^4)
S = matrix(kk, {{1,0,0,0},
                {1,1,0,0},
                {1,1,1,0},
                {1,1,1,1}})
M = S * M1
(P,L,U) = LUdecomposition M
M == L*U

S = matrix(kk, {{1,0,0,0},
                {1,0,0,1},
                {1,1,0,0},
                {1,1,1,0}})
M = S * M1
(P,L,U) = LUdecomposition M
assert(M == (permutationMatrix P) * L * U)
L*U
M
M == checkLU LUdecomposition M
--------- ones above do NOT check LUdecomposition...

-- empty matrix (reported by Joel Louwsma in Zulip)
scan({map(RR^0, RR^0, {}), map(CC^0, CC^0, {})}, M -> (
    (P,L,U) := LUdecomposition M;
    assert((permutationMatrix P) * L * U == M);
    assert isUnitLowerTriangular L;
    assert isREF U;
	assert Equation(rank M, 0);
	assert Equation(det M, 1)))

----------------------
-- git issue 3361 ----
----------------------
-- Note: over ZZ/p, GF q, QQ, flint is used directly.
-- I think this means it is doing something smart for QQ...
M = map(ZZ^12, ZZ^12,{{-5728, 0, 14099, -14819, -13434, -12548, -12641, -2879, 8002, 1306, 13728, 12071}, {0, 12971, 0, 0, 0, -11213, 0, 0, 0, 0, 0, 0}, {-11213, -14159, -2647,
0, 0, 0, 9175, 2283, 2148, -11161, 11430, -8584}, {0, 2432, 12538, -13653, -11213, 0, -14860, 0, -11161, -12361, 4945, -8975}, {-773, 0, -6019, 12232, -3991, 4986, 13820, -15133, 7392, -11679,
-14964, -204}, {0, 9638, 0, 0, 0, 5691, 0, 0, 0, 0, 0, 0}, {5691, -7352, -5048, 0, 0, 0, -14667, 6940, 1529, 3455, 2112, -14149}, {0, -7544, -3961, 2669, 5691, 0, 3470, 0, 3455, -5892, 1235, 8824},
{139, 0, 7768, 11670, 9388, -11760, -2343, 11075, 3172, 1206, -871, 3079}, {0, -4039, 0, 0, 0, 14236, 0, 0, 0, 0, 0, 0}, {14236, 14089, -5483, 0, 0, 0, 4599, -3525, -6542, 7415, 10050, -4055}, {0,
6825, 1369, 9198, 14236, 0, 14239, 0, 7415, -2616, -13371, 8183}})

kk = ZZ/32003
A = sub(M, kk)
A1 = mutableMatrix A
redA = reducedRowEchelonForm A1 -- this is correct
isRREF redA
matrix redA -- this is WRONG in 1.24.11...
assert(mutableMatrix entries redA - redA == 0) -- failed in 1.24.11
  -- they look the same, but a mismatch of what layout is is different
  -- between flint and M2 (in some versions up through 1.24.11)
(P,L,U) = LUdecomposition A
    assert((permutationMatrix P) * L * U == A);
    assert isUnitLowerTriangular L;
    assert isREF U;
    rowRankProfile mutableMatrix A
    columnRankProfile mutableMatrix A  

kk = GF (27)
A = sub(M, kk)
A1 = mutableMatrix A
redA = reducedRowEchelonForm A1 -- this is correct
matrix redA -- this is WRONG!!!!!
assert(mutableMatrix entries redA - redA == 0)
LUdecomposition A
rowRankProfile mutableMatrix A
columnRankProfile mutableMatrix A  


kk = GF (101,2)
A = sub(M, kk)
A1 = mutableMatrix A
redA = reducedRowEchelonForm A1 -- this is correct
matrix redA -- this is WRONG!!!!!
assert(mutableMatrix entries redA - redA == 0)
LUdecomposition A
rowRankProfile mutableMatrix A
columnRankProfile mutableMatrix A  

kk = QQ
A = sub(M, kk)
A1 = mutableMatrix A
redA = reducedRowEchelonForm A1 -- this is correct
matrix redA
assert(mutableMatrix entries redA - redA == 0)
(P,L,U) = LUdecomposition A -- TOO MUCH DERBUGGING OUTPUT!!
  assert(#P == numcols A)
  assert isPermutation P
  assert isUnitLowerTriangular L
  assert((permutationMatrix P) * L * U == A)
rowRankProfile mutableMatrix A
columnRankProfile mutableMatrix A  

A = M
A1 = mutableMatrix A
assert try(reducedRowEchelonForm A1; false) else true -- not implemented, good!

kk = RR_53
A = sub(M, kk)
A1 = mutableMatrix A
--redA = reducedRowEchelonForm A1 -- not implemented
--matrix redA
--assert(mutableMatrix entries redA - redA == 0)
rowRankProfile mutableMatrix A
columnRankProfile mutableMatrix A  

------------------------------
-- checking the permutation returned.
-- It might not be consistent!
I = id_(QQ^5)
E = I_{0,2,3,1,4}
(P,L,U) = LUdecomposition E
assert((permutationMatrix P) * L * U == E)

I = id_((ZZ/32003)^5)
E = I_{0,2,3,1,4}
(P,L,U) = LUdecomposition E
assert((permutationMatrix P) * L * U == E)

I = id_(RR^5)
E = I_{0,2,3,1,4}
(P,L,U) = LUdecomposition E
assert((permutationMatrix P) * L * U == E)

I = id_(RR_300^5)
E = I_{0,2,3,1,4}
(P,L,U) = LUdecomposition E
assert((permutationMatrix P) * L * U == E)

----------------------------
-- git issue 3361. Fixed. --
----------------------------
  kk = ZZ/32003
  A = map(kk^12, kk^12, {{-5728, 0, 14099, -14819, -13434, -12548, -12641, -2879, 8002, 1306, 13728, 12071}, {0, 12971, 0, 0, 0, -11213, 0, 0, 0, 0, 0, 0}, {-11213, -14159, -2647,
0, 0, 0, 9175, 2283, 2148, -11161, 11430, -8584}, {0, 2432, 12538, -13653, -11213, 0, -14860, 0, -11161, -12361, 4945, -8975}, {-773, 0, -6019, 12232, -3991, 4986, 13820, -15133, 7392, -11679,
-14964, -204}, {0, 9638, 0, 0, 0, 5691, 0, 0, 0, 0, 0, 0}, {5691, -7352, -5048, 0, 0, 0, -14667, 6940, 1529, 3455, 2112, -14149}, {0, -7544, -3961, 2669, 5691, 0, 3470, 0, 3455, -5892, 1235, 8824},
{139, 0, 7768, 11670, 9388, -11760, -2343, 11075, 3172, 1206, -871, 3079}, {0, -4039, 0, 0, 0, 14236, 0, 0, 0, 0, 0, 0}, {14236, 14089, -5483, 0, 0, 0, 4599, -3525, -6542, 7415, 10050, -4055}, {0,
6825, 1369, 9198, 14236, 0, 14239, 0, 7415, -2616, -13371, 8183}})

  R = reducedRowEchelonForm A
  assert(submatrix(R, {0..10}, {0..10}) == 1) -- fails in 1.24.11
  assert(submatrix(R, {11}, ) == 0) -- fails in 1.24.11

  B = map(kk^16, kk^19,{{8569, 0, 10278, 1503, 6334, -4220, -9148, 15585, -94, -10202, 2995, 5704, -1750, -807, -14613, 10231, 5544, -4899, 2405}, {0, -1134, 0, 0, 0, 10639, 0, 0,
0, 0, 0, 0, 0, 4508, -6408, 6174, -10053, 14858, 7799}, {10639, -7038, -1400, 0, 0, 0, -6408, 9016, 12189, -2287, 4545, -13516, -14504, 3087, 0, 0, 7799, -5827, 13280}, {0, 11645, -14690, -12816,
10639, 0, 4508, 0, -2287, -10053, 1657, -12559, 12569, 0, 3087, 0, 0, -12102, 0}, {14866, 0, -9973, 2478, 10892, 2953, 3958, 7071, -12656, -326, -8513, -4867, -11818, -13189, -8419, 10484, 4078,
7922, -7801}, {0, -8066, 0, 0, 0, 11963, 0, 0, 0, 0, 0, 0, 0, -5460, 8322, 7768, 12885, -7506, -11298}, {11963, 64, -15855, 0, 0, 0, 8322, -10920, -7691, -15012, -3616, 9004, 11745, 3884, 0, 0,
-11298, 2948, -14691}, {0, -11004, 15815, -15359, 11963, 0, -5460, 0, -15012, 12885, -4665, 8014, -12290, 0, 3884, 0, 0, -5649, 0}, {2116, 0, -7959, 2868, -10974, -14133, 6516, 13599, 1856, 5288,
-5852, 2639, -15911, 2775, -13454, -5936, 15454, -6341, -12322}, {0, -13246, 0, 0, 0, 11135, 0, 0, 0, 0, 0, 0, 0, 3791, -11887, 7527, 13701, -15187, -7307}, {11135, 8706, 6473, 0, 0, 0, -11887, 7582,
-15643, 1629, 9674, 11410, -2926, -12238, 0, 0, -7307, 6319, -742}, {0, -13945, 4876, 8229, 11135, 0, 3791, 0, 1629, 13701, -3061, -10775, -9788, 0, -12238, 0, 0, 12348, 0}, {-15344, 0, -9878, 6885,
7948, 2022, 4731, -10889, -1886, -7585, -4728, -3709, 762, -705, 11069, 12069, 10303, -206, -294}, {0, -5446, 0, 0, 0, -2767, 0, 0, 0, 0, 0, 0, 0, 1851, 4439, 11117, 10640, -3409, -8177}, {-2767,
10704, -5791, 0, 0, 0, 4439, 3702, 4861, -6818, 4033, 5893, -2364, -10443, 0, 0, -8177, 5384, 5509}, {0, 7610, 8183, 8878, -2767, 0, 1851, 0, -6818, 10640, -6739, 10451, -11904, 0, -10443, 0, 0,
11913, 0}})

  BR = reducedRowEchelonForm B
  assert(submatrix(BR, {0..15}) == 1) -- fails in 1.24.11

  kk = ZZ/101
  C = map(kk^16, kk^16,{{9560, 0, -9456, -14529, -12617, -10590, 6197, -12617, -3617, -3801, -7917, 2227, 2275, -14043, -7099, 9296}, {0, 12081, 0, 0, 0, 14699, 0, 0, 0, 0, 0, 0, 0,
8174, 2154, -13021}, {14699, 12896, 1820, 0, 0, 0, 2154, -15655, 10185, 9550, -14749, -1629, 12043, 9491, 0, 0}, {0, -14636, -14970, 4308, 14699, 0, 8174, 0, 9550, 10424, -1086, -14630, 11410, 0,
9491, 0}, {-537, 0, -5529, -3869, 7893, -2874, -15124, -14784, -9737, 5455, 5219, 7150, -9579, 12416, 12563, 656}, {0, 11533, 0, 0, 0, -9771, 0, 0, 0, 0, 0, 0, 0, 10608, 877, 10029}, {-9771, 7909,
5138, 0, 0, 0, 877, -10787, -4484, -3781, -2782, -2234, -13392, -10987, 0, 0}, {0, -6503, 12135, 1754, -9771, 0, 10608, 0, -3781, -1848, -12157, -10394, 13051, 0, -10987, 0}, {-6215, 0, 15213, 3528,
6243, 12476, -8541, -6999, -11266, -11750, 5900, -9385, 10445, -11532, -11727, 9998}, {0, -7410, 0, 0, 0, 12299, 0, 0, 0, 0, 0, 0, 0, 13533, -9974, -15007}, {12299, 4293, 8356, 0, 0, 0, -9974, -4937,
441, -10136, 1353, 4108, 2950, 8498, 0, 0}, {0, -15524, -10931, 12055, 12299, 0, 13533, 0, -10136, -12876, -7929, -9302, -13796, 0, 8498, 0}, {1737, 0, -7177, -6310, -4930, -13898, 6962, -8503,
-11634, 7176, 7410, -923, 11490, 3125, 3309, 14885}, {0, 323, 0, 0, 0, 2513, 0, 0, 0, 0, 0, 0, 0, 193, 6564, 14691}, {2513, -10341, 9192, 0, 0, 0, 6564, 386, 7212, -14261, -3324, -15212, 3705, -8656,
0, 0}, {0, 4097, 3470, 13128, 2513, 0, 193, 0, -14261, -13064, 11194, 14406, 10360, 0, -8656, 0}})

  assert(reducedRowEchelonForm C == 1) -- fails in 1.24.11

  KK = ZZ/32003
  A = mutableMatrix {
    {0_KK,-3144,14801,-3224,-6137,4996,3765,-11088},
    {-14457,0,-2027,1882,1761,3501,11180,3512},
    {-6448,-5854,0,-8601,3765,0,12333,14493},
    {0,3767,-9072,-13943,3608,-5031,9798,-13757},
    {938,0,1817,-7827,-4924,4382,-2038,11803},
    {4117,5207,0,-4536,9798,0,-1677,-15300},
    {0,-775,2162,15141,-4072,-7356,-9685,-1629},
    {-9290,0,-12133,8844,2155,9377,7353,5284},
    {-1721,10776,0,1081,-9685,0,-2452,-2900},
    {0,-6037,-8195,-13216,5028,8547,-3401,-6949},
    {-5505,0,-12724,1394,-11062,8766,5237,-12122},
    {5571,7093,0,11904,-3401,0,2849,-6914}}

  B = reducedRowEchelonForm transpose A
  transpose B
  assert(transpose entries B == entries transpose B) -- fails in 1.24.11

------------------------------
-- git issue #2975.  Fixed. --
------------------------------
  K =  matrix {{1, 1, 0, 0}, {0, 0, 1, 0}, {0, 0, 0, 1}, {0, 0, 1, 1}}
  KQ = sub(K, QQ)
  (P, L, U) = LUdecomposition KQ
  Q = id_(QQ^4)_P
  assert(Q*L*U == KQ) -- note: is is *not equal to K.  Those are over different rings.
  assert(K != KQ)
  assert(Q*L*U - K == 0)  

------------------------------
-- git issue #2941.
------------------------------
  a = mutableMatrix{{0.0,1.0,2.0,3.0,0.0,0.0,0.0,0.0},{4.0,3.0,2.0,1.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,1.0,2.0,3.0,4.0},{0.0,0.0,0.0,0.0,3.0,2.0,1.0,0.0}}
  (sig, u, vt) = SVD a
  sigma = mutableMatrix(RR_53, 4, 8)
  for i from 0 to numrows sig-1 do sigma_(i,i) = sig_(i,0)
  sigma
  u * sigma * vt - a
  a * (transpose vt)_{4..7}

  ring a
  LUdecomposition a
  rowRankProfile a
  columnRankProfile a
  b = nullSpace a
  ring b
  --assert(a * b == 0) -- might only be true approximately...
  assert(clean(1e-14, matrix a * syz matrix a) == 0)
  assert(clean(1e-14, a * b) == 0)

  a = mutableMatrix{
      {0.0,1.0,2.0,3.0,0.0,0.0,0.0,0.0},
      {4.0,3.0,2.0,1.0,0.0,0.0,0.0,0.0},
      {0.0,0.0,0.0,0.0,1.0,2.0,3.0,4.0},
      {0.0,0.0,0.0,0.0,3.0,2.0,1.0,0.0},
      {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0},
      {0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0}
      }
  LUdecomposition a
  rowRankProfile a
  columnRankProfile a

  a=mutableMatrix{{0.0,1.0,2.0,3.0,0.0,0.0,0.0,0.0},{4.0,3.0,2.0,1.0,0.0,0.0,0.0,0.0},{0.0,0.0,0.0,0.0,1.0,2.0,3.0,4.0},{0.0,0.0,0.0,0.0,3.0,2.0,1.0,0.0}}
  (P,L,U) = LUdecomposition matrix a
  assert((permutationMatrix P) * L * U == matrix a)
  assert isUnitLowerTriangular L
  -- assert isREF U -- FALSE. Lapack does not give REF form...
  assert(rowRankProfile a == {0, 1, 2, 3}) -- these seem ok...
  assert(columnRankProfile a == {0, 1, 4, 5}) -- these seem ok...

------------------------------
---- test over frac(QQ[x]) ---
------------------------------
  S = frac(QQ[x])
  R = matrix(S, {{1,0,x,0,x^2+1},{0,1,-1/x,0,2*x},{0,0,0,1,7*x}})
  B = matrix(S, {{1,2*x,3},{0,3,-x},{1,2*x^2,0}})
  A = B * R

  --reducedRowEchelonForm A -- not working yet
  (P,L,U) = LUdecomposition A
  assert(L*U - A == 0) -- note that L*U != A because of grading....  Urrgh....

  S = frac(QQ[x, DegreeRank => 0])
  R = matrix(S, {{1,0,x,0,x^2+1},{0,1,-1/x,0,2*x},{0,0,0,1,7*x}})
  B = matrix(S, {{1,2*x,3},{0,3,-x},{1,2*x^2,0}})
  A = B * R
  (P,L,U) = LUdecomposition A
  assert(L*U == A)
