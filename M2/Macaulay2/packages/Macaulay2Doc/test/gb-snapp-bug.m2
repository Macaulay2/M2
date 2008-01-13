R = QQ[x_1..x_4,MonomialOrder => Lex]
I = ideal(x_1^2 + x_1*x_4+1,x_1*x_2*x_3*x_4+1)
gens gb(I, BasisElementLimit => 2)
assert(gens gb I == gens gb ideal(x_1^2 + x_1*x_4+1,x_1*x_2*x_3*x_4+1))
