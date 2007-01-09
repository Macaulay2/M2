A = QQ[i]/(i^2+1);
toField A
B = A[x,y,z]
I = ideal(i*x^2-y-i, i*y^2-z-i)
gens gb I
