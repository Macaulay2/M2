R = QQ[x,y,z,w]
assert(support ideal"x,xy,xy-zw" == R_*)
assert(support ideal() == {})
assert(support id_(ZZ^3) == {})

