W = QQ[x,y,D_x,D_y,h,WeylAlgebra=>{x=>D_x,y=>D_y,h},MonomialOrder=>{Weights=>{1,1,1,1}, Weights=>{1,1,-1,-1}}]
describe W
value toExternalString W -- check that this doesn't fail
assert(value toExternalString vector {1,2,3} == vector {1,2,3})
assert(value toExternalString interval(1, 2) == interval(1, 2))
