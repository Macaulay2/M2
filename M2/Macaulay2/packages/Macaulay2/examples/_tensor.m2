M = monoid[a..d, MonomialOrder => Eliminate 1]
N = monoid[e,f,g, Degrees => {1,2,3}]
P = tensor(M,N,MonomialOrder => GRevLex)
describe P
tensor(M,M,Variables => {t_0 .. t_7}, MonomialOrder => ProductOrder{4,4})
describe oo
tensor(ZZ/101[x,y], ZZ/101[r,s], MonomialOrder => Eliminate 2)
