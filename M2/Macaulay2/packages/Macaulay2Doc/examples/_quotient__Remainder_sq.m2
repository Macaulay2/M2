R = ZZ[x,y]
f = random(R^{2:1},R^2)
g = transpose (vars R ++ vars R)
(q,r) = quotientRemainder'(f,g)
q*g+r == f
f = f + map(target f, source f, id_(R^2))
(q,r) = quotientRemainder'(f,g)
q*g+r == f
