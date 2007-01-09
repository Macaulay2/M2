R = ZZ/101[a..d]
I = intersect((ideal(a,b,c^3-d^3))^2,ideal(a^2-c^2,b^2-d^2))
gbTrace=3
gb(I, BasisElementLimit=>5)
gbSnapshot I
gb(I, BasisElementLimit=>10)
gbSnapshot I
gens gb I
