errorDepth 0
k = ZZZ/5
print k
o = monomialOrdering(RevLex => 4)
print o
print (o ** o)
M = monoid [x,y,z,t, NewMonomialOrder => RevLex => 4]
print M
print see M
print see M.MonomialOrder
print options M
R = k M
print R
print see R
engineStack()
f = (1+x+y^3)^3
print leadMonomial f
print baseName t
print listForm f
print standardForm f
