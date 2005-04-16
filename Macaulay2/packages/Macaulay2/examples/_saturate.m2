R = ZZ/32003[a..d];
I = ideal(a^3-b, a^4-c)
Ih = homogenize(I,d)
saturate(Ih,d)
m = ideal vars R
M = R^1 / (a * m^2)
M / saturate 0_M
