R = ZZ[x,y]
assert ( 0 == codim ( R^2, Generic => true) )
assert ( 0 == codim (ideal 0_R, Generic => true))
assert ( 0 == codim ( cokernel map(R^3, R^0, 0) , Generic => true) )
assert ( 1 == codim (ideal ( 2*x ), Generic => true ) )
assert ( 2 == codim (ideal ( 2*x , 3*y ), Generic => true ) )
assert ( infinity == codim (R^0, Generic => true ) )
assert ( infinity == codim (ideal 1_R, Generic => true))
f = (M,N) -> assert( codim (M++N, Generic => true) == min(codim(M, Generic => true), codim(N, Generic => true)) )
f(quotient module ideal x, quotient module ideal(x,y))
f(quotient module ideal x, quotient module ideal 0_R)
f(quotient module ideal 1_R, quotient module ideal(x,y))

R = QQ[x,y]
assert ( 0 == codim ( R^2 ) )
assert ( 0 == codim (ideal 0_R ))
assert ( 0 == codim ( cokernel map(R^3, R^0, 0)  ) )
assert ( 1 == codim (ideal ( 2*x )  ) )
assert ( 2 == codim (ideal ( 2*x , 3*y )  ) )
assert ( infinity == codim (R^0  ) )
assert ( infinity == codim (ideal 1_R ))
f(quotient module ideal x, quotient module ideal(x,y))
f(quotient module ideal x, quotient module ideal 0_R)
f(quotient module ideal 1_R, quotient module ideal(x,y))

R2=(R/ideal(x,y))[t];
assert ( 1 == codim ideal t)
