R = ZZ/32003[a..d,MonomialOrder=>Lex];
m = matrix{{a*d, b^2, b^100, b^50*d^50, c^2*d}}
p = sortColumns m
m_p
p = sortColumns(m, DegreeOrder=>null, MonomialOrder=>Descending)
m_p
R = ZZ/101[a..c];
f = matrix{{1,a,a^2,b^2,b,c,c^2,a*b,b*c,a*c}}
s = sortColumns f
f_s
s = sortColumns(f,DegreeOrder => Descending)
f_s
