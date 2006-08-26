R = ZZ/32003[a..e];
f = matrix{{a^2, b^2, c^2, d^2, e^3, a*d-e^2}}
g = toDual(1,f)
ideal fromDual g == ideal f
g = toDual(2,f)
ideal fromDual g == ideal f
g = toDual(3,f)
ideal fromDual g == ideal f
