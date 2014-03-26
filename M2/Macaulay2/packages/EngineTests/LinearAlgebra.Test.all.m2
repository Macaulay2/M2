export {
    fields,
    ringsPID,
    ringsRR,
    ringsCC,
    tests,
    runTests
    }
    
debug Core
fields = {
    "ZZp 2", 
    "ZZp 3",
    "ZZp 5", 
    "ZZp 101", 
    "ZZp 32719",
    "ZZpFlint 2",
    "ZZpFlint 5",
    "ZZpFlint 101",
    "ZZpFlint 4611686018427387847",
    "ZZpFlint 9223372036854775783",
    "ZZpFFPACK 2",
    "ZZpFFPACK 3",
    "ZZpFFPACK 5",
    "ZZpFFPACK 101",
    "ZZpFFPACK 30000001",
    "ZZpFFPACK maxFFPACKPrime",
    "GF(3,2)",
    "GF(5,12)",
    ///GF(3,2, Strategy=>"New")///,
    ///GF(2,7, Strategy=>"New")///,
    ///GF(3,2, Strategy=>"Givaro")///,
    ///GF(2,7, Strategy=>"Givaro")///,
    ///GF(3,2, Strategy=>"CompleteGivaro")///,
    ///GF(2,7, Strategy=>"CompleteGivaro")///,
    "QQ",
    "QQFlint"
    }
ringsPID = {
    "ZZ",
    "ZZFlint"
    }
ringsRR = {
    "RR_53",
    "RR_54",
    "RR_100",
    "RR_134",
    "RR_200",
    "RR_256",
    "RR_1000",
    "RR_10000"
    }
ringsCC = {
    "CC_53",
    "CC_54",
    "CC_100",
    "CC_134",
    "CC_200",
    "CC_256",
    "CC_1000",
    "CC_10000"
    }

tests = {
    "testDeterminant",
    "testInverse",
    "testMult",
    "testRank",
    "testNullspace",
    "testLUBoundaryCases"
    --"testRankProfile"
    --"testSolve"
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
  runTests(fields, tests, set {
          ("QQFlint", "testRank"), 
          ("QQFlint", "testNullspace")
          })
///

end

debug Core
runTests(fields, tests, set {("QQFlint", "testRank")})
runTests(fields, tests, set {})
runTests(fields, {"testLUBoundaryCases"}, set {("QQFlint", "testRank")})
runTests({"QQFlint"}, {"testNullspace"}, set {})
runTests(ringsPID_{1}, tests_{1,3})

-- QQFlint: testrank:  takes alot of time, why?
--    testNullspace: bus error!
-- ZZ: determinant: fails
-- ZZpFlint: 
--    LUboundary cases fail
--    because rowRankPrifle not implemented
-- ZZpFFPACK
--    det of 0x0 should be 1 (it is 0!)
--    inverse of 0x0 matrix crashes.
-- RR_53
--    crash in testLUBoundaryCases
-- RR_100
--    rank: uses wrong algorithm, for matrices...
ringsPID/testDeterminant -- fails for ZZ, not for ZZFlint

testDeterminant QQ
testRank QQ
testMult QQ

fields/value
kk = value fields_12
raw kk
testLUBoundaryCases kk

kk = RR_53
testLUBoundaryCases kk

debug Core
runTests(fields, tests_{0}, set {}) -- passes
runTests(fields, tests_{1}, set {}) -- passes
runTests(fields, tests_{2}, set {}) -- passes
runTests(fields, tests_{3}, set {})  --QQFlint takes way too long, but all run to completion.
  -- Question: does QQFlint actually call flint code here?
runTests(fields, tests_{4}, set {})  --QQFlint crashes
runTests(fields, tests_{5}, set {})  -- testLUBoundaryCases

-- Flint issues: solveLinear (during 'solve'), rankProfile.
  -- solveLinear for possibly non-invertible matrices needs to be done
  -- rankProfile: needs to be written too
  -- QQFlint:
  --   nullSpace: needs to be written
  --   solveLinear
  --   rankProfile