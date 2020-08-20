-- This was a bug in versions <= -r8263
-- The // was missing a factor of 2, arising from 
-- normal form of elements and use of "use_denom,denom" in engine.

S = QQ [a, b, c, x,y, MonomialOrder => {1,2,2}]
I = ideal"2ay+by6-cx2+cxy-2cy2,
          2ax-by6+cx2-cxy,
	  4a2-b2y10+bcx2y4-bcxy5+6bcy6-4c2x2+2c2xy-4c2y2"
R = S/I
F = matrix"xy+y2"
M = matrix"c2y4"
G = M // F
assert(F * G == M)
f = F_(0,0)
m = M_(0,0)
g = m // f
assert(g == G_(0,0)) 
assert(f * g == m)
