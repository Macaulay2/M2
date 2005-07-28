rsort {4,2,3,1}
R = QQ[a..d];
m = matrix{{a*b, c*d, a*d^3, b^3*c, 1_R}}
rsort(m, DegreeOrder=>Ascending)
