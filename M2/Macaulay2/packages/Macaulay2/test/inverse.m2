
-- Also in the included file is some strange behavior of
-- units in a ring I was doing some matrix computations in.
-- In this case, R = ZZ/101[x,y,z,w]/ideal{x*y,z*w}, and
-- inputting (2_R)^(-1) gives a result of 2, even though
-- its inverse in R is -50. Performing lift(1_R/2_R, R)
-- does indeed give -50, and so I did this temporarily,
-- but I thought you may want to know about this problem too.
-- 
-- Thanks,
-- 
-- Frank Moore
-- UNL Math Dept

R = ZZ/101[x,y,z,w]/ideal{x*y,z*w}
assert ( 2 * 2_R^-1 == 1 )

k = ZZ/101
assert(2_k^-1 == -50)
R=k[x]
assert(2_R^-1 == -50)
S = ZZ/101[x,y,z,w]/ideal{x*y,z*w}
assert(2_S^-1 == -50)

assert(1//x == 0)
assert try x^-1 else true

f = matrix {{1.,2,3},{4,6,9},{2,4,8}}
f * f^-1 == 1.
