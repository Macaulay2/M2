R = ZZ/101[x_(0,0)..x_(2,2),y_(0,0)..y_(2,2)]
M = genericMatrix(R,x_(0,0),3,3)
N = genericMatrix(R,y_(0,0),3,3)
I = ideal flatten(M*N-N*M);
dim I
needsPackage "SimplicialComplexes"
R = QQ[a..d]
D = simplicialComplex {a*b*c,a*b*d,a*c*d,b*c*d}
I = monomialIdeal D
facets D
dim D
dim I
