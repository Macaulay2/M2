restart
R = ZZ/101[a..d]
I = ideal random(R^1, R^{-2}^4)
J = ideal gens gb I
see J
J = ideal J_*
options forceGB
G = forceGB(gens J, Options => 1)
gbTrace=3
gens G
gb gens J
gens gb J

J = ideal"-b2-ac, 3ac-6bc, ab-13b2-c2, 12a2-3c2, c2d-ad2, bcd-d3, bcd-c2d"
G = forceGB(gens J, Options => 32)
gens G
