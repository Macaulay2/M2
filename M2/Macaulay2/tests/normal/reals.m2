rings = {ZZ, QQ, RR, CC}
table(rings, rings, (R, S) -> (
	assert(13_R // 4_S - 3.0 < 1e-15);
	assert(13_R %  4_S - 1.0 < 1e-15);
	));

scan({ZZ, QQ, RR, CC}, F -> (
	assert(3_F // (pi*ii) < 1e-15);
	assert(3_F %  (pi*ii) == 3_F);
	assert((pi*ii) // 3_F < 1e-15);
	assert((pi*ii) %  3_F == pi*ii);
	))

assert((pi*ii) // (3*ii) == 1)
assert((pi*ii) %  (3*ii) + 3*ii == pi*ii)

R = ZZ[x]
f = (2*x+2)^3
assert same {(x+1)^3, f // 8, f / 8}
assert same {(x+1)*x, f // 16}
assert(try f / 16 then false else true)

R = QQ[x]
f = (2*x+2)^3
assert same {(x+1)^3,   f // 8,  f / 8}
assert same {(x+1)^3/2, f // 16, f / 16}
