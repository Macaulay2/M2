export {
    tests,
    runTests
    }
    
debug Core -- for ZZp

tests = {
    "testDeterminant",
    "testInverse",
    "testMult",
    "testRank",
    "testNullspace",
    "testSolve",
    "testRankProfile",
    "testLUBoundaryCases",
    "testMutableMatrices"
    --"testLU"
    --"testRREF"
    }

runTests = (rings, tests, exceptions) -> (
    R := rings/(r -> (r,value r));
    T := tests/(t -> (t,value t));
    for t in T do for r in R do  (
        if member((r#0,t#0), exceptions) then (
            << "xcep " << t#0 << " ring " << r#0 << endl;
            )
        else (
            << "test " << t#0 << " ring " << r#0 << " time ";
            tim := timing (t#1(r#1));
            << tim#0 << endl;
            )
        )
    )

TEST ///
  debug Core
  time runTests(fields, tests, set {
          ("QQ", "testLUBoundaryCases")
          })
///

TEST ///
  -- This is a crashing strange memory type bug
  debug loadPackage "EngineTests"
  R = ZZpFFPACK 2
  for i from 1 to 100 do
  (<< "i="<< i<<endl; testRankProfile R; debug Core)

///

TEST ///
-- linear algebra over ZZ is not functional, really.  Partly because the
-- tests assume the base ring is a field...
  debug Core
  runTests(ringsPID, {"testMult","testDeterminant","testRank"}, set{
          ("ZZ", "testDeterminant"),
          ("ZZ", "testRank"),
          ("ZZFlint", "testRank")
          })

///

end

debug Core

-- QQFlint: testrank:  takes alot of time, why?
--    testNullspace: bus error!
-- ZZ: determinant: fails
-- ZZpFlint: 
--    LUboundary cases fail
--    because rowRankProfle not implemented
-- RR_53
--    crash in testLUBoundaryCases
-- RR_100
--    rank: uses wrong algorithm, for matrices...

testDeterminant QQ
testRank QQ
testMult QQ


-- Flint issues: solveLinear (during 'solve'), rankProfile.
  -- solveLinear for possibly non-invertible matrices needs to be done
  -- rankProfile: needs to be written too
  -- QQFlint:
  --   nullSpace: needs to be written
  --   solveLinear
  --   rankProfile
  --   the rank function is taking a huge amount of time, why?
  

loadPackage "EngineTests"
debug Core
R = QQFlint
M =  entries random(ZZ^2, ZZ^4)
M = mutableMatrix(R,2,4)
M_(0,0) = 3
M_(1,0) = 2
M_(1,1) = 5
rawLinAlgRREF raw M
M = mutableMatrix(R, 10, 20)
fillMatrix M
rawLinAlgRREF raw M
mutableMatrix(ZZFlint, 5, 5)
fillMatrix oo
map(QQFlint, ZZFlint)
oo (matrix o26)
mutableMatrix oo
rawLinAlgRREF raw oo

M = mutableMatrix(QQ, 100, 100)
M1 = mutableMatrix(QQFlint, 100, 100)
M2 = mutableMatrix(ZZ, 100, 100)
M3 = mutableMatrix(ZZFlint, 100, 100)

for i from 0 to 99 do for j from 0 to 99 do (
    a := random 100;
    M_(i,j) = a;
    M1_(i,j) = a;
    M2_(i,j) = a;
    M3_(i,j) = a;
    )
M1
time det M
time det M1
time det M2 -- crashes?  takes forever in any case...
time det M3
time (M*M);
time (M1*M1);
time (M2*M2);
time (M3*M3);

N = matrix M;
N1 = matrix M1;
N2 = matrix M2;
N3 = matrix M3;

-- These times are awful
time (N*N);
time (N1*N1);
time (N2*N2);
time (N3*N3);
time det N;
time det N1;
time det N2;
time det N3;
time det M3;

-- problems here:
  -- det over (regular) ZZ: very bad, seems screwed up
  -- QQFlint: implement LU, solve, rankProfile
    -- switch out QQGMP?
  -- ZZFlint: 
    -- switch out RingZZ?
-- remove files not being used.
-- change name from MatrixOppies.
-- use mutableMatrix routines for ZZ, QQ, etc.  What is needed to do that?
  -- a. make the matrix functions virtual
  -- b. make a new matrix class for each of these (or one templated one?)
  -- c. det class should use this version of det if possible?
  -- difficulties: sparse vs dense matrices.
  
runTests(fields, {"testLUBoundaryCases"}, set{
        ("ZZpFlint 2", "testLUBoundaryCases"),
        ("ZZpFlint 5", "testLUBoundaryCases"),
        ("ZZpFlint 101", "testLUBoundaryCases"),
        ("ZZpFlint 4611686018427387847", "testLUBoundaryCases"),
        ("ZZpFlint 9223372036854775783", "testLUBoundaryCases"),
        ("QQ", "testLUBoundaryCases")
        })  
