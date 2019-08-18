-- test of some of the internal printing routines done in the engine.

debug Core
A = ZZ/32003[s,t]
B = A[x,y]
F = (-3*s-2)*x^2-x*y+1
assert(toString raw F == "-3x2s-2x2-xy+1")
assert(toString raw (x*y) == "xy")
assert(toString raw (-x*y) == "-xy")
assert(toString raw 1_B == "1")
assert(toString raw (-1_B) == "-1")

A = ZZ[s,t]
B = A[x,y]
F = (-3*s-2)*x^2-x*y+1
assert(toString raw F == "-3x2s-2x2-xy+1")
assert(toString raw (x*y) == "xy")
assert(toString raw (-x*y) == "-xy")
assert(toString raw 1_B == "1")
assert(toString raw (-1_B) == "-1")

A = QQ[s,t]
B = A[x,y]
F = (-3*s-2)*x^2-x*y+1
assert(toString raw F == "-3x2s-2x2-xy+1")
assert(toString raw (x*y) == "xy")
assert(toString raw (-x*y) == "-xy")
assert(toString raw 1_B == "1")
assert(toString raw (-1_B) == "-1")

A = QQ[s,t]
B = A[x,y]/(s^3+t^3)
F = (-3*s-2)*x^2-x*y+1
assert(toString raw F == "-3x2s-2x2-xy+1")
assert(toString raw (x*y) == "xy")
assert(toString raw (-x*y) == "-xy")
assert(toString raw 1_B == "1")
assert(toString raw (-1_B) == "-1")

A = RR_53[s,t]
B = A[x,y]
F = (-3*s-2)*x^2-x*y+1
assert(toString raw F == "-3x2s-2x2-xy+1")
assert(toString raw (x*y) == "xy")
assert(toString raw (-x*y) == "-xy")
assert(toString raw 1_B == "1")
assert(toString raw (-1_B) == "-1")
