-- Fast nonminimal resolutions are not implemented over QQ yet
R = QQ[x,y,z]
I = ideal(x^2-y*z, x^4)
assert(try(res(I, FastNonminimal=>true);false) else true) -- should fail until implemented
C = res(I, Strategy=>4.1)


