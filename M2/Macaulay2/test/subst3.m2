R = QQ[x]
c = promote(1/2,R)
assert( substitute ( c , {x => 0_R} ) == c )
