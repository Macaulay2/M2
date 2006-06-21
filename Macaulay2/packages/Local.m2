-- under development by dan

newPackage "Local"
export {LocalRing,localRing,ambientRing,maxIdeal}
LocalRing = new Type of EngineRing
debug Core
presentation LocalRing := (R) -> presentation R.ambientRing
generators LocalRing := (R) -> generators R.ambientRing
isCommutative LocalRing := (R) -> isCommutative R.ambientRing
degreeLength LocalRing := (R) -> degreeLength R.ambientRing
numgens LocalRing := (R) -> numgens R.ambientRing
-- promote(ZZ,LocalRing) := promote(RingElement,LocalRing) := (r,R) -> promote(r,R.ambientRing)

localRing = method()

localRing(EngineRing,Ideal) := (R,m) -> (					    -- R = poly ring, m = max ideal
     S := new LocalRing from raw R;
     S.ambientRing = R;
     S.maxIdeal = m;
     S.baseRings = append(R.baseRings, R);
     expression S := s -> expression lift(s,R);
     S)
endPackage "Local"

-- end
-- try it out immediately

errorDepth = 0
R = localRing (QQ[x,y], ideal(x,y))
M = R^4
C = res M
