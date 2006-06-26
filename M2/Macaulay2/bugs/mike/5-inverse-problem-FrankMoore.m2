--- other problem
R = ZZ/101[x,y,z,w]/ideal{x*y-z*w}
--- returns 2(incorrect)
(2_R)^(-1)
--- returns -50 (correct)
(1/2)_R
promote(1/2,R)
1_R/2_R
assert(lift(1_R/2_R,R) == -50)
