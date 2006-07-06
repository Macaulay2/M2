f = matrix "2;0"
f%f							    -- this crashes, sometimes

R = QQ[x]
assert( x // 0 === 0 )
assert( x % 0 === x )
f = matrix(R,{{x}})
g = matrix(R,{{0}})
assert( f // g == 0 )
assert( f % g == f )
