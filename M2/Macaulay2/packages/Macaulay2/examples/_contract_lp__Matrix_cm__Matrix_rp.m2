R = ZZ[x,y,z]
f = vars R ** vars R
contract(transpose vars R, f)
contract(x, f)
contract(y, f)
contract(z, f)
