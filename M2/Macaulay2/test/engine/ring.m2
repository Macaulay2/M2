R = ZZ[x,y,z]
R'= raw R
M = monoid R
M'= raw M
F = R^3
F'= raw F
G = F**F
raw G
assert( rank G == 9 )
v = vars R
v
v_(0,0)
v_(0,1)
v_(0,2)
entries v
transpose v
v || v

-- Local Variables:
-- compile-command: "make -C $M2BUILDDIR/Macaulay2/test/engine ring.okay"
-- End:
