R=QQ[x]/ideal(x^2)
q=ideal(x)
G=associatedGradedRing q
dim G
assert( dim G == 0 )

S=QQ[x,y,z,s]
k=intersect(ideal(x^3, z^2), ideal(y^4, s^2), ideal(x+z, y-z))
R=S/k
q=ideal(x-s+y, s+z)
radical q
G=associatedGradedRing q
assert( dim G == 2 )
