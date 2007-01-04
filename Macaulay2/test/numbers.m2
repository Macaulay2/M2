-- Test of moving numbers from one ring to another,
-- and matrices of such too.

-- rings considered: ZZ, QQ, RR, RRR, CC, CCC

assert try (lift(1.3,ZZ); false) else true
M = matrix{{1.3,2.4567}}
M = matrix{{1.0,2.0}}
lift(M,RR,ZZ)
