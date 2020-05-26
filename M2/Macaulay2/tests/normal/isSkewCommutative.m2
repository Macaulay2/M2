R = QQ[x,y,z]
assert( not isSkewCommutative R )
R = QQ[x,y,z,WeylAlgebra => {x => y}]
assert( not isSkewCommutative R )
R = QQ[x,y,z,SkewCommutative => true]
assert( isSkewCommutative R )
R = QQ[x,y,z,SkewCommutative => {x,y}]
assert( isSkewCommutative R )
R = QQ[x,y,z]/z^2
assert( not isSkewCommutative R )
R = QQ[x,y,z,WeylAlgebra => {x => y}]/z^2
assert( not isSkewCommutative R )
R = QQ[x,y,z,SkewCommutative => true]/z^2
assert( isSkewCommutative R )
R = QQ[x,y,z,SkewCommutative => {x,y}]/z^2
assert( isSkewCommutative R )
