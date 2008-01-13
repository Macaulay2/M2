R = QQ[x,y,z];
f = x^2*y*z+x*y^2*z+y^2*z+z^3+x*y;
f1 = x*y+y^2-1
f2 = x*y
G = ideal(f1,f2)
f % G
f % (forceGB gens G)
f % (forceGB matrix{{f2,f1}})
