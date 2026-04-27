restart
R = ZZ/101[a..d]
I = ideal(a^2-b*c, a^3-c*d-1)
gb(I, Algorithm => ParallelF4)
