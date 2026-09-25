kk = ZZ/11
R = kk[a..d, SkewCommutative => true]
S = kk[s, t]

-- homogeneous
f = map(R, S, {a*b*c*d, a*b + c*d})
assert(ker f == ideal(t^2-2*s, s*t, s^2))

-- inhomogeneous
g = map(R, S, {a*b*c*d + 2*a*b, c*d + 1})
assert(ker g == ideal(t^2-2*t+1,s^2))