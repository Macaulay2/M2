R = ZZ/10007[a,b];
f = (2*a+3)^4 + 5
g = (2*a+b+1)^3
size f, size g
degree f
degree g
terms g
select(terms g, i -> degree i == {2})
sum oo
part(0,g)
part(1,g)
part(2,g)
part(3,g)
toString f
toString g
quot = f//g
rem = f%g
f == quot * g + rem
homogenize(f,b)
ring f
ring f === ring g
part(1,f)
f_a
g_(a*b)
leadTerm g
leadCoefficient g
leadMonomial g
exponents leadMonomial g
coefficients f
coefficients g
exponents f
exponents g
listForm f
S = listForm g
S / print;
S = standardForm f
standardForm g
S#(new HashTable from {0 => 2})
f < g
sort {b^2-1,a*b,a+1,a,b}
f ? g
