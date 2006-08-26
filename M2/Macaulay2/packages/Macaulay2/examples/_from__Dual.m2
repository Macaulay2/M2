R = ZZ/32003[x_1..x_3];
g = random(R^1, R^{-4})
f = fromDual g
res ideal f
betti oo
