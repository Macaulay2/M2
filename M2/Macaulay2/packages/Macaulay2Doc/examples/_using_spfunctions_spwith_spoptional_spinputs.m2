R = ZZ/101[x,y,z,w];
gb ideal(x*y-z^2,y^2-w^2)
gens oo
options gb
gb(ideal(x*y-z^2,y^2-w^2), DegreeLimit => 2)
gens oo
(options gb).Syzygies
