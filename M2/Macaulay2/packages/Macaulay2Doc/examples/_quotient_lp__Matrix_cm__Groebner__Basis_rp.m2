R = ZZ[x,y]
f = random(R^2,R^{2:-1})
g = vars R ++ vars R
quotient(f,g)
f = f + map(target f, source f, id_(R^2))
quotient(f,g)
