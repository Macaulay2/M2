R = ZZ/101[a..h]
p = genericMatrix(R,a,2,4)
q = generators gb p
C = resolution cokernel leadTerm q
betti C
degrees C_2
t2 = tally degrees C_2
peek t2
t2_{2}
t2_{3}
