-- Some possible issues with rings

A = QQ[a,b,c]/(a^2-b^2)
B = A[x,y]/(a*x+b*y,x^3,y^3)
describe A
value toExternalString A
use A
toExternalString B
presentation B
ambient B
-- how to get B as a poly ring mod an ideal?
C = QQ[generators(B, CoefficientRing=>QQ)]
F = map(B, C, generators(B, CoefficientRing=>QQ))
ker F -- BUG: not implemented yet!

D = QQ[gens C, Degrees => {{1,0},{1,0},{0,1},{0,1},{0,1}}]
G = map(B, D, generators(B, CoefficientRing=>ZZ))
ker G -- BUG: not implemented yet!

-- ring maps with different degrees:
A1 = QQ[a,b,c,Degrees=>{{1,0},{0,1},{1,1}}]
G1 = map(A,A1,vars A)
ker G1 -- seems OK

-- ring maps involving fractions
A = frac(QQ[s,t])
B = QQ[x,y,z]
G = map(A,B,{s/t, (s^2-1)/(t^2-2), s^3/(t+1)})
J = ker G -- OK now
assert(G (J_0) == 0)
