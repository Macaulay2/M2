R = ZZ/101[a,b,c]
I = ideal (b*c, a*c, a*b)
b = basis_3 I
assert( rank source b == 7 )
