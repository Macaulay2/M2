-- fixed: 4/25/2010

assert( 97^2 < (options GF).SizeLimit )
k = GF 97^2
assert isHomogeneous matrix 1_k
assert( 101^2 > (options GF).SizeLimit )
k = GF 101^2
-- tell Dimitri Markouchevitch <Dimitri.Markouchevitch@math.univ-lille1.fr> when this works:
assert isHomogeneous matrix 1_k				  -- error, unexplained

R = QQ[x, y, z]/(-z^9+x*y^2)
S = R [w_0, w_1, w_2, Degrees => {{1, 1}, {1, 1}, {1, 1}}, Heft => {0, 1}]
I = ideal(z*w_1-y*w_2,z*w_0-x*w_2,y*w_0-x*w_1,x*y*w_1-z^8*w_2,x*w_1^2-z^7*w_2^2,w_0*w_1^2-z^6*w_2^3)
C = res I						    -- used to crash
scan(0 .. 4,  { 1, 6, 11, 19, 20 }, (i,r) -> assert( numgens C_i == r ))

kk = ZZ/101
kk[x, DegreeRank=>0]
f = x^2-1
assert isHomogeneous f
