R = ZZ/101[a..c];
f = matrix{{1,a,a^2,b^2,b,c,c^2,a*b,b*c,a*c}}
s = sortColumns f
f_s
s = sortColumns(f,DegreeOrder => Descending)
f_s
