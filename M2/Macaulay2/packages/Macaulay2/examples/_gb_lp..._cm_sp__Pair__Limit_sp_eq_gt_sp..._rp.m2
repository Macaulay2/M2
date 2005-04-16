R = QQ[x,y,z,w]
I = ideal(x*y-z,y^2-w-1,w^4-3)
gb(I, PairLimit => 1)
gb(I, PairLimit => 2)
gb(I, PairLimit => 3)
