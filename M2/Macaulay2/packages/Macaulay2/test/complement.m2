R = ZZ[x,y]
f = matrix {{-28, 113*x-38*y, 103*x^2-51*x*y+161*y^2}, {46, 43*x+6*y, -9*x*y+43*y^2}, {94, 73*x+10*y, -45*x^2-49*x*y+63*y^2}}
g = complement f
-- g == matrix {{-14, -3, 0}, {23, 5, 0}, {47, 0, -1}}
isSurjective (f | g)
for i from 0 to numgens source g - 1 do assert not isSurjective (f | submatrix'(g,{i}))
