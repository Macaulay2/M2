-- [ 1509468 ] Computation Hang-up and Strange Behavior of Units
--   (?)
-- Submitted By:
-- Frank Moore - fmoore 	Date Submitted:
-- 2006-06-20 17:37
-- Last Updated By:
-- fmoore - Attachment added


Q = ZZ/32003[a,b,c,d]
J1 = ideal {a^3,b^3,c^3,d^3}
J2 = ideal {a^4,b,c,d}
J3 = ideal {a,b^4,c,d}
I = intersect(J1,J2,J3)
--- define quotient ring over Q
use Q
S = Q/(I+ideal{b^3})
res coker vars S
--- define quotient ring over Q another way
use Q
S = Q/ideal {a^4,a^3*b,a^3*c,a^3*d,b^3,c^3,d^3}
res coker vars S
--- define intermediate ring R and then define S as a quotient ring of R
--- hangs up on res coker vars S
R = Q/I
K = ideal {b^3}
S = R/K
-- gbTrace = 3
C = res coker vars S  -- goes to higher and higher degree without stopping: FIXED: 6/20/06

net betti C == net "total: 1 4 13 41 129 406
    0: 1 4  6  4   1   .
    1: . .  3 12  18  12
    2: . .  4 22  58  93
    3: . .  .  3  36 150
    4: . .  .  .  16 127
    5: . .  .  .   .  24"
    

