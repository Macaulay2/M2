R = QQ[x,y]/(x^2-y^3)
f = map(frac R, QQ[t_0..t_2], {x/1,y/1,x/y})
I = kernel f
assert( (t_0^2 - t_1^3) % I == 0 )
J = trim I
assert( (t_0^2 - t_1^3) % J == 0 )
