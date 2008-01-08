--status: new code converting between various types of numbers needs to be implemented to make this one work

-- Test of moving numbers from one ring to another,
-- and matrices of such too.

-- rings considered: ZZ, QQ, RR, CC

assert try (lift(1.3,ZZ); false) else true
M = matrix{{1.3,2.4567}}
M = matrix{{1.0,2.0}}
--status: this is waiting for Mike to implement this case in the engine, in rawLift
lift(M,RR,ZZ)


debug Core
rawFromNumber(raw RR, 1.2)
rawFromNumber(raw CC, 1.2)
