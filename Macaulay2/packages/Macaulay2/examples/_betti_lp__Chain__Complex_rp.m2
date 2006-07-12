R = ZZ/101[a..h]
p = genericMatrix(R,a,2,4)
q = generators gb p
C = resolution cokernel leadTerm q
betti C
