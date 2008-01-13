kk = ZZ/101
A = kk[a,b]
B = kk[c,d,e]
describe(A**B)
describe tensor(A,B,VariableBaseName=>p)
describe tensor(A,B,Variables=>{a1,a2,b1,b2,b3})
describe (C = tensor(A,B,DegreeRank=>1,Degrees=>{5:1}))
degreeLength C
degreesRing C
describe tensor(A,B,MonomialSize=>8)
describe (C = tensor(A,B,MonomialOrder=>Eliminate numgens A))
describe (C = tensor(A,B,MonomialOrder=>GRevLex)) 
As = kk[a,b,SkewCommutative=>true]
D = kk[c,d,e,SkewCommutative=>true]
E = tensor(As,D)
describe E
c*a
E = kk[x,Dx,WeylAlgebra=>{x=>Dx}]
tensor(E,E,Variables=>{x,Dx,y,Dy})
describe oo
A = ZZ/101[a,b]
B = A[x,y]
tensor(B,B,Variables=>{x1,y1,x2,y2})
describe oo
