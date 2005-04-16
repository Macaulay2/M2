R = ZZ/101[x,y,z]
f = x+2*y+3*z
substitute(f,{x=>x^3, y=>y^3})
S = ZZ/101[z,y,x]
substitute(f,S)
