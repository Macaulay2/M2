--status: this old test depends on internal things and probably should be deleted


---------------------------------------------------
-- Test of engine free module code ----------------
---------------------------------------------------
-- Also tests whether these are connected at top level correctly

needs "raw-util.m2"
errorDepth = 0

-- rawRing, rawRank, rawMultiDegree
-- rawFreeModule(R,5), rawFreeModule(R,(0,1,2,3))
-- rawDirectSum, rawTensor
-- rawDual, rawSymmetricPower, rawExteriorPower, rawSubmodule
-- toString
R = polyring(rawZZ(), (symbol a .. symbol f))
F = rawFreeModule(R,5)
assert(rawRank F === 5)
assert(rawMultiDegree F === {0,0,0,0,0})
assert(rawRing F === R)
G = rawFreeModule(R, (1,2,3,4))
assert(rawMultiDegree G === {1,2,3,4})
H = rawFreeModule(R,0)
assert(rawRank H === 0)
assert(rawMultiDegree H === {})
assert try (rawFreeModule(R,-1);false) else true
rawRank H
G = rawFreeModule(R,(0,0,1,1,-3))
assert(toString G === "free(rank 5 degrees = {1, 1, t, t, t^(-3)})")
assert(rawRank(F ++ G) === rawRank F + rawRank G)
assert(rawMultiDegree(F ++ G) === join(rawMultiDegree F, rawMultiDegree G))

F = rawFreeModule(R,(0,10,100,1000))
G = rawFreeModule(R,(1,2,3))
H = rawTensor(F,G)
assert(F ** G === H)
assert(rawRank H === rawRank F * rawRank G)
assert(rawMultiDegree H === 
     {1, 2, 3, 11, 12, 13, 101, 102, 103, 1001, 1002, 1003})
assert(rawMultiDegree rawDual F === - rawMultiDegree F)
assert(rawMultiDegree rawDual H === - rawMultiDegree H)
assert(rawMultiDegree rawExteriorPower(3,F) === {110,1010,1100,1110})
assert(rawSubmodule(F, (0,0,1,1,2,2)) === rawFreeModule(R,(0,0,10,10,100,100)))
S2F = rawSymmetricPower(2,F)
assert(rawRank S2F === 10)
assert(toString S2F === 
     "free(rank 10 degrees = {1, t10, t100, t1000, t20, t110, t1010, t200, t1100, t2000})")

assert(rawMultiDegree rawExteriorPower(3,F) === {110,1010,1100,1110})
assert(rawSubmodule(F, (0,0,1,1,2,2)) === rawFreeModule(R,(0,0,10,10,100,100)))
assert(rawRank rawSubmodule(F, (0,1)) === 2)
assert(rawRank rawSubmodule(F, ()) === 0)

assert(rawRank rawSymmetricPower(-1,F) === 0)
assert(rawSymmetricPower(0,F) === R^1)
assert(rawSymmetricPower(1,F) === F)
assert(rawRank rawExteriorPower(-1,F) == 0)
assert(rawExteriorPower(0,F) == R^1)
assert(rawExteriorPower(1,F) == F)

assert(R^4 == rawFreeModule(R,4))
assert(R^4 === rawFreeModule(R,4))
R^{-1,-1,2} == rawFreeModule(R,(1,1,-2))

print "ERROR: need to implement hash for RawFreeModule"
print "WARNING: test rawFreeModule RawMatrix"
print "WARNING: test rawGetSchreyer"

-- Local Variables:
-- compile-command: "M2 -e errorDepth=0 --stop -e 'load \"raw-freemodule.m2\"' -e 'exit 0' "
-- End:
