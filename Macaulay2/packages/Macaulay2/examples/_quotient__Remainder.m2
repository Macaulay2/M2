R = ZZ[x,y]
f = random(R^2,R^{2:-1})
g = vars R ++ vars R
(q,r) = quotientRemainder(f,g)
g*q+r == f
f = f + map(target f, source f, id_(R^2))
(q,r) = quotientRemainder(f,g)
g*q+r == f
