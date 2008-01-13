R = QQ[x,y,z]
S = QQ[t,u]
f = map(R,S,{x*y,y*z})
assert( preimage_f ideal(x,y,z) == ideal(t,u) )
assert( preimage_f ideal(x^2,y^2) == ideal(u^2,t*u,t^2) )
