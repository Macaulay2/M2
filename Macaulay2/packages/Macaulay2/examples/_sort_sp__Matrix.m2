R = ZZ/32003[a..d,MonomialOrder=>Lex];
m = matrix{{a*d, b^2, b^100, b^50*d^50, c^2*d}}
sort m
options sort
sort(m, DegreeOrder=>null, MonomialOrder=>Descending)
