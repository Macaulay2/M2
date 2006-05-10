R1 = ZZ/101[x_0 .. x_10]
R3 = ZZ/101[x_0 .. x_8][x_9,x_10]

F3 = map(R3,R1)
F3inv = map(R1,R3) -- fails
F3 * F3inv
assert((F3inv * F3) (vars R1) ==  vars R1)
